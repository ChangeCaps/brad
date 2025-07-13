#!/usr/bin/env -S uv run --script
# -*- coding: utf-8 -*-
# /// script
# require-python =  ">=3.12"
# dependencies = [
#     "pandas>=2.0",
#     "numpy>=1.24",
#     "scipy>=1.11",
#     "matplotlib>=3.8",
#     "seaborn>=0.13",
#     "psutil>=5.9",
#     "pydantic>=2.0",
#     "pyarrow>=15.0",
#     "tabulate>=0.9.0",
#     "click>=8.0",
#     "Jinja2",
# ]
# ///

"""
BRAD Language Benchmark Suite

A comprehensive, statistically rigorous benchmarking framework for the BRAD
programming language. Supports multiple compilation targets with statistical
analysis, memory profiling, and automated report generation.

Features:
- Statistical rigor with 30+ samples and confidence intervals
- Memory and compilation time measurement
- Executable size analysis
- Pandas DataFrame output for LaTeX export
- Performance graphs and visualizations
"""

import os
import shutil
import signal
import subprocess
import sys
import threading
import time
from abc import ABC, abstractmethod
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import List, Dict, Optional, Tuple

import click
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import psutil
import scipy.stats as stats
import seaborn as sns
from pydantic import BaseModel, Field, field_validator
from tabulate import tabulate

# Constants
BRAD_FILE_EXTENSION = ".bd"
# Get the directory where this script is located (bench/)
SCRIPT_DIR = Path(__file__).parent
DEFAULT_OUTPUT_DIR = SCRIPT_DIR / ".results"
DEFAULT_ITERATIONS = 30
DEFAULT_WARMUP_RUNS = 3
DEFAULT_TIMEOUT = 60
DEFAULT_EXECUTION_TIMEOUT = 30
DEFAULT_CONFIDENCE = 0.95
MIN_SUCCESSFUL_RUNS = 3


def discover_brad_files(paths: List[Path]) -> List[Path]:
    """Discover and validate all .bd files from the given paths (files or directories)"""
    brad_files = []

    for path in paths:
        if not path.exists():
            click.secho(f"❌ Path does not exist: {path}", fg="red")
            continue

        if path.is_file():
            if path.suffix == BRAD_FILE_EXTENSION:
                # Additional validation: check if file is readable
                try:
                    with path.open("r") as f:
                        f.read(1)  # Try to read first byte
                    brad_files.append(path)
                except (PermissionError, OSError) as e:
                    click.secho(f"⚠️  Cannot read file {path}: {e}", fg="yellow")
            else:
                click.secho(
                    f"⚠️  Skipping {path}: not a {BRAD_FILE_EXTENSION} file", fg="yellow"
                )

        elif path.is_dir():
            # Recursively find all .bd files in directory
            found_files = list(path.rglob(f"*{BRAD_FILE_EXTENSION}"))
            if not found_files:
                click.secho(
                    f"⚠️  No {BRAD_FILE_EXTENSION} files found in directory: {path}",
                    fg="yellow",
                )
            else:
                # Validate each found file
                for file_path in found_files:
                    try:
                        with file_path.open("r") as f:
                            f.read(1)  # Try to read first byte
                        brad_files.append(file_path)
                    except (PermissionError, OSError) as e:
                        click.secho(
                            f"⚠️  Cannot read file {file_path}: {e}", fg="yellow"
                        )
        else:
            click.secho(f"⚠️  Unknown path type: {path}", fg="yellow")

    return sorted(set(brad_files))


class BenchmarkTarget(str, Enum):
    """Available BRAD compilation targets"""

    LUA = "lua"
    SIMPLE_SPEC = "simple-spec-pipeline"


class ErrorType(str, Enum):
    """Types of errors that can occur during benchmarking"""

    SUCCESS = "success"
    TIMEOUT = "timeout"
    SEGFAULT = "segfault"
    INVALID_EXIT_CODE = "invalid_exit_code"
    COMPILATION_ERROR = "compilation_error"
    EXECUTION_ERROR = "execution_error"
    MEMORY_ERROR = "memory_error"
    UNKNOWN_ERROR = "unknown_error"


@dataclass
class BenchmarkResult:
    """Single benchmark execution result"""

    target: str
    file_path: str
    compilation_time: float
    execution_time: float
    memory_peak_mb: float
    executable_size_bytes: int
    exit_code: int
    stdout: str
    stderr: str
    iteration: int
    error_type: ErrorType
    error_message: str
    timed_out: bool
    signal_received: Optional[int] = None


@dataclass
class BenchmarkStatistics:
    """Statistical analysis of benchmark results"""

    mean: float
    median: float
    std_dev: float
    confidence_interval: Tuple[float, float]
    coefficient_variation: float
    outlier_count: int
    sample_size: int
    min_value: float
    max_value: float


class BenchmarkConfig(BaseModel):
    """Configuration for benchmark execution"""

    iterations: int = Field(default=DEFAULT_ITERATIONS, ge=5, le=200)
    warmup_runs: int = Field(default=DEFAULT_WARMUP_RUNS, ge=0, le=10)
    timeout_seconds: int = Field(default=DEFAULT_TIMEOUT, gt=0)
    execution_timeout_seconds: int = Field(default=DEFAULT_EXECUTION_TIMEOUT, gt=0)
    confidence_level: float = Field(default=DEFAULT_CONFIDENCE, gt=0.0, lt=1.0)
    remove_outliers: bool = Field(default=True)
    fail_fast: bool = Field(default=False)
    max_consecutive_failures: int = Field(default=5, ge=1)
    parallel_workers: int = Field(default=0, ge=0, le=64)  # 0 = auto (cores/2)

    @field_validator("confidence_level")
    @classmethod
    def validate_confidence(cls, v: float) -> float:
        if not 0.0 < v < 1.0:
            raise ValueError("Confidence level must be between 0 and 1")
        return v

    def get_worker_count(self) -> int:
        """Get the actual number of workers to use"""
        if self.parallel_workers == 0:
            # Default to half the available cores, minimum 1
            return max(1, os.cpu_count() // 2)
        return self.parallel_workers


class TargetRunner(ABC):
    """Abstract base class for benchmark target runners"""

    def __init__(self, working_dir: Path, config: BenchmarkConfig):
        self.working_dir = working_dir
        self.config = config

    @abstractmethod
    def get_compilation_command(self, file_path: Path) -> List[str]:
        """Get the compilation command for this target"""
        pass

    @abstractmethod
    def get_execution_command(self, file_path: Path) -> Optional[List[str]]:
        """Get the execution command for this target (None if no separate execution)"""
        pass

    @abstractmethod
    def get_output_files(self, file_path: Path) -> List[Path]:
        """Get list of output files generated by compilation"""
        pass

    @abstractmethod
    def measure_executable_size(self, file_path: Path) -> int:
        """Measure the size of generated executable/output"""
        pass

    @abstractmethod
    def supports_execution_timing(self) -> bool:
        """Whether this target supports separate execution timing"""
        pass

    def get_target_name(self) -> str:
        """Get the display name for this target"""
        return self.__class__.__name__.replace("Runner", "").lower()

    def cleanup_output_files(self, file_path: Path) -> None:
        """Clean up any generated output files"""
        for output_file in self.get_output_files(file_path):
            if output_file.exists():
                try:
                    output_file.unlink()
                except OSError:
                    pass


class LuaRunner(TargetRunner):
    """Runner for BRAD -> Lua compilation"""

    def __init__(self, working_dir: Path, config: BenchmarkConfig):
        super().__init__(working_dir, config)
        # Create a unique temporary file for this runner instance
        import time
        import os

        timestamp = int(time.time() * 1000000)  # microsecond timestamp
        pid = os.getpid()
        thread_id = threading.get_ident()
        self.lua_file = self.working_dir / f"temp_lua_{pid}_{thread_id}_{timestamp}.lua"

    def get_compilation_command(self, file_path: Path) -> List[str]:
        return [
            "cargo",
            "run",
            "v2",
            "lua",
            "-o",
            str(self.lua_file),
            "lib/std",
            str(file_path),
        ]

    def get_execution_command(self, file_path: Path) -> Optional[List[str]]:
        if self.lua_file.exists():
            return ["lua", str(self.lua_file)]
        return None

    def get_output_files(self, file_path: Path) -> List[Path]:
        return [self.lua_file]

    def measure_executable_size(self, file_path: Path) -> int:
        if self.lua_file.exists():
            return self.lua_file.stat().st_size
        return 0

    def supports_execution_timing(self) -> bool:
        return True

    def get_target_name(self) -> str:
        return "lua"

    def cleanup_output_files(self, file_path: Path) -> None:
        """Enhanced cleanup for Lua files"""
        for output_file in self.get_output_files(file_path):
            if output_file.exists():
                try:
                    output_file.unlink()
                except OSError as e:
                    # Log cleanup issues but don't fail the benchmark
                    click.secho(
                        f"Warning: Could not clean up {output_file}: {e}", fg="yellow"
                    )


class SimpleSpecRunner(TargetRunner):
    """Runner for BRAD simple specification pipeline"""

    def get_compilation_command(self, file_path: Path) -> List[str]:
        return ["cargo", "run", "v2", "simple-spec-pipeline", str(file_path)]

    def get_execution_command(self, file_path: Path) -> Optional[List[str]]:
        # Simple spec pipeline doesn't produce executable output for timing
        return None

    def get_output_files(self, file_path: Path) -> List[Path]:
        # Simple spec pipeline might generate debug/intermediate files
        # Add any known output files here
        return []

    def measure_executable_size(self, file_path: Path) -> int:
        # Simple spec pipeline doesn't produce measurable output
        return 0

    def supports_execution_timing(self) -> bool:
        return False

    def get_target_name(self) -> str:
        return "simple-spec-pipeline"


class TargetRunnerRegistry:
    """Registry for managing different target runners"""

    def __init__(self):
        self._runners: Dict[str, type] = {}
        self._register_builtin_runners()

    def _register_builtin_runners(self):
        """Register built-in runners"""
        self.register(BenchmarkTarget.LUA, LuaRunner)
        self.register(BenchmarkTarget.SIMPLE_SPEC, SimpleSpecRunner)

    def register(self, target: BenchmarkTarget, runner_class: type) -> None:
        """Register a runner class for a target"""
        self._runners[target.value] = runner_class

    def create_runner(
            self, target: BenchmarkTarget, working_dir: Path, config: BenchmarkConfig
    ) -> TargetRunner:
        """Create a runner instance for the given target"""
        if target.value not in self._runners:
            raise ValueError(f"No runner registered for target: {target.value}")

        runner_class = self._runners[target.value]
        return runner_class(working_dir, config)

    def get_available_targets(self) -> List[str]:
        """Get list of available target names"""
        return list(self._runners.keys())


class BradBenchmarkRunner:
    """Main benchmark runner for BRAD language"""

    def __init__(self, config: BenchmarkConfig = None):
        self.config = config or BenchmarkConfig()
        self.results: List[BenchmarkResult] = []
        self.working_dir = Path.cwd()
        self._progress_lock = threading.Lock()
        self._progress_callback = None
        self.runner_registry = TargetRunnerRegistry()
        self.log_dir = None  # Will be set when benchmarking starts

        # Ensure we're in the right directory
        if not (self.working_dir / "Cargo.toml").exists():
            raise RuntimeError("Must be run from BRAD project root directory")

    def register_runner(self, target: BenchmarkTarget, runner_class: type) -> None:
        """Register a custom runner for a target"""
        self.runner_registry.register(target, runner_class)

    def get_available_targets(self) -> List[str]:
        """Get list of available benchmark targets"""
        return self.runner_registry.get_available_targets()

    def setup_logging(self, output_dir: Path) -> None:
        """Setup logging directory structure for debugging failures"""
        # Ensure output directory exists first
        output_dir.mkdir(parents=True, exist_ok=True)

        self.log_dir = output_dir / "logs"
        self.log_dir.mkdir(parents=True, exist_ok=True)

        # Create subdirectories for each target and file combination
        # This will be done dynamically as needed

    def cleanup_old_results(self, output_dir: Path) -> None:
        """Clean up old benchmark results before starting new run"""
        if output_dir.exists():
            try:
                shutil.rmtree(output_dir)
                click.secho(
                    f"   🧹 Cleaned up old results in {output_dir}", fg="yellow"
                )
            except Exception as e:
                click.secho(f"   ⚠️  Could not clean up old results: {e}", fg="yellow")

    def _get_log_file_path(
            self, target: BenchmarkTarget, file_path: Path, iteration: int
    ) -> Path:
        """Get log file path for a specific benchmark run"""
        if not self.log_dir:
            raise RuntimeError("Logging directory not set.")

        # Create directory structure: logs/target/file_name/
        file_name = file_path.stem  # Remove .bd extension
        target_dir = self.log_dir / target.value / file_name
        target_dir.mkdir(parents=True, exist_ok=True)

        # Create filename: iteration_001_stdout.log or iteration_001_stderr.log
        if iteration < 0:  # Warmup run
            filename = f"warmup.log"
        else:
            filename = f"iteration_{iteration:03d}.log"

        return target_dir / filename

    def run_single_benchmark(
            self, target: BenchmarkTarget, file_path: Path
    ) -> List[BenchmarkResult]:
        """Run benchmark for single file and target with statistical sampling"""
        worker_count = self.config.get_worker_count()

        if worker_count == 1:
            return self._run_benchmark_sequential(target, file_path)
        else:
            return self._run_benchmark_parallel(target, file_path, worker_count)

    def _run_benchmark_sequential(
            self, target: BenchmarkTarget, file_path: Path
    ) -> List[BenchmarkResult]:
        """Run benchmark sequentially (original implementation)"""
        click.echo(f"🔬 {file_path.name} ({target.value}): ", nl=False)

        results = []
        consecutive_failures = 0

        # Warmup runs (silent)
        for i in range(self.config.warmup_runs):
            result = self._execute_single_run(target, file_path, -1)

        # Measurement runs
        successful_runs = 0
        for iteration in range(self.config.iterations):
            result = self._execute_single_run(target, file_path, iteration)
            results.append(result)

            if result.error_type == ErrorType.SUCCESS:
                successful_runs += 1
                consecutive_failures = 0
                click.secho("●", fg="green", nl=False)
            else:
                consecutive_failures += 1
                click.secho("✗", fg="red", nl=False)

                # Fail fast if too many consecutive failures
                if (
                        self.config.fail_fast
                        and consecutive_failures >= self.config.max_consecutive_failures
                ):
                    click.secho(
                        f" stopped after {consecutive_failures} consecutive failures",
                        fg="red",
                    )
                    break

        success_rate = successful_runs / len(results) if results else 0
        success_color = (
            "green" if success_rate > 0.8 else "yellow" if success_rate > 0.5 else "red"
        )
        click.secho(f" {successful_runs}/{len(results)} successful", fg=success_color)

        if successful_runs < MIN_SUCCESSFUL_RUNS:
            raise RuntimeError(
                f"Too few successful runs: {successful_runs} (minimum: {MIN_SUCCESSFUL_RUNS})"
            )

        return results

    def _run_benchmark_parallel(
            self, target: BenchmarkTarget, file_path: Path, worker_count: int
    ) -> List[BenchmarkResult]:
        """Run benchmark with parallel execution"""
        click.echo(
            f"🔬 {file_path.name} ({target.value}, {worker_count} workers): ", nl=False
        )

        # Warmup runs (sequential to avoid resource conflicts)
        for i in range(self.config.warmup_runs):
            result = self._execute_single_run(target, file_path, -1)
        results = []
        successful_runs = 0
        consecutive_failures = 0

        # Use ThreadPoolExecutor for parallel execution
        with ThreadPoolExecutor(max_workers=worker_count) as executor:
            # Submit all tasks
            future_to_iteration = {
                executor.submit(self._execute_single_run, target, file_path, i): i
                for i in range(self.config.iterations)
            }

            # Process completed tasks
            for future in as_completed(future_to_iteration):
                try:
                    result = future.result()
                    results.append(result)

                    if result.error_type == ErrorType.SUCCESS:
                        successful_runs += 1
                        consecutive_failures = 0
                        with self._progress_lock:
                            click.secho("●", fg="green", nl=False)
                    else:
                        consecutive_failures += 1
                        with self._progress_lock:
                            click.secho("✗", fg="red", nl=False)

                        # Fail fast if too many consecutive failures
                        if (
                                self.config.fail_fast
                                and consecutive_failures
                                >= self.config.max_consecutive_failures
                        ):
                            click.secho(
                                f" stopped after {consecutive_failures} consecutive failures",
                                fg="red",
                            )
                            # Cancel remaining futures
                            for remaining_future in future_to_iteration:
                                if not remaining_future.done():
                                    remaining_future.cancel()
                            break

                except Exception:
                    with self._progress_lock:
                        click.secho("!", fg="yellow", nl=False)
                    continue

        # Sort results by iteration to maintain order
        results.sort(key=lambda r: r.iteration)

        success_rate = successful_runs / len(results) if results else 0
        success_color = (
            "green" if success_rate > 0.8 else "yellow" if success_rate > 0.5 else "red"
        )
        click.secho(f" {successful_runs}/{len(results)} successful", fg=success_color)

        if successful_runs < MIN_SUCCESSFUL_RUNS:
            raise RuntimeError(
                f"Too few successful runs: {successful_runs} (minimum: {MIN_SUCCESSFUL_RUNS})"
            )

        return results

    def _execute_single_run(
            self, target: BenchmarkTarget, file_path: Path, iteration: int
    ) -> BenchmarkResult:
        """Execute single benchmark run with comprehensive monitoring and error handling"""

        # Get the appropriate runner for this target
        try:
            runner = self.runner_registry.create_runner(
                target, self.working_dir, self.config
            )
        except ValueError as e:
            return self._create_error_result(
                target, file_path, iteration, ErrorType.UNKNOWN_ERROR, str(e)
            )

        # Get compilation command from runner
        cmd = runner.get_compilation_command(file_path)

        # Initialize result variables
        process = None
        start_memory = psutil.virtual_memory().used
        compilation_time = 0.0
        execution_time = 0.0
        memory_samples = [start_memory]
        exit_code = -1
        stdout = ""
        stderr = ""
        error_type = ErrorType.SUCCESS
        error_message = ""
        timed_out = False
        signal_received = None

        try:
            # Clean up any existing output files
            runner.cleanup_output_files(file_path)

            log = self._get_log_file_path(target, file_path, iteration)

            # Measure compilation time
            compile_start = time.perf_counter()

            # Execute command with timeout
            process = subprocess.Popen(
                cmd,
                stdout=log.open('a'),
                stderr=subprocess.STDOUT,
                text=True,
                cwd=self.working_dir,
                preexec_fn=os.setsid,  # Create new process group for better cleanup
            )

            # Monitor memory usage and stream output with timeout
            psutil_process = psutil.Process(process.pid)
            timeout_start = time.time()

            # Monitor memory and timeout while streaming
            while process.poll() is None:
                # Check for timeout
                if time.time() - timeout_start > self.config.timeout_seconds:
                    timed_out = True
                    self._terminate_process_group(process)
                    error_type = ErrorType.TIMEOUT
                    error_message = (
                        f"Compilation timed out after {self.config.timeout_seconds}s"
                    )
                    break

                # Sample memory usage
                try:
                    memory_info = psutil_process.memory_info()
                    memory_samples.append(memory_info.rss + start_memory)
                    time.sleep(0.01)  # Sample every 10ms
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    break

            # Get final result
            if not timed_out:
                compile_end = time.perf_counter()
                compilation_time = compile_end - compile_start
                exit_code = process.returncode

                # Analyze exit code and signals
                error_type, error_message, signal_received = self._analyze_exit_code(
                    exit_code
                )

            # Measure execution time if the runner supports it
            if (
                    runner.supports_execution_timing()
                    and error_type == ErrorType.SUCCESS
                    and exit_code == 0
                    and not timed_out
            ):
                execution_time, exec_error_type, exec_error_msg = (
                    self._measure_execution_time(runner, file_path, target, iteration)
                )
                if exec_error_type != ErrorType.SUCCESS:
                    error_type = exec_error_type
                    error_message = exec_error_msg

            # Measure executable size using runner
            executable_size = (
                runner.measure_executable_size(file_path)
                if error_type == ErrorType.SUCCESS
                else 0
            )

            # Calculate peak memory usage
            peak_memory_mb = (
                (max(memory_samples) - start_memory) / 1024 / 1024
                if memory_samples
                else 0
            )

            return BenchmarkResult(
                target=target.value,
                file_path=str(file_path),
                compilation_time=compilation_time,
                execution_time=execution_time,
                memory_peak_mb=peak_memory_mb,
                executable_size_bytes=executable_size,
                exit_code=exit_code,
                stdout=stdout[:1000],  # Truncate for storage
                stderr=stderr[:1000],
                iteration=iteration,
                error_type=error_type,
                error_message=error_message,
                timed_out=timed_out,
                signal_received=signal_received,
            )

        except Exception as e:
            # Handle unexpected errors
            if process:
                self._terminate_process_group(process)

            return self._create_error_result(
                target,
                file_path,
                iteration,
                ErrorType.UNKNOWN_ERROR,
                f"Unexpected error: {str(e)}",
            )

        finally:
            # Always clean up temporary files after each iteration
            runner.cleanup_output_files(file_path)

    def _terminate_process_group(self, process: subprocess.Popen):
        """Safely terminate a process and its children"""
        if process.poll() is None:
            try:
                # Try to terminate the entire process group
                os.killpg(os.getpgid(process.pid), signal.SIGTERM)

                # Wait a bit for graceful shutdown
                try:
                    process.wait(timeout=2)
                except subprocess.TimeoutExpired:
                    # Force kill if graceful shutdown failed
                    os.killpg(os.getpgid(process.pid), signal.SIGKILL)
                    process.wait()
            except (ProcessLookupError, OSError):
                # Process already terminated
                pass

    def _analyze_exit_code(
            self, exit_code: int
    ) -> Tuple[ErrorType, str, Optional[int]]:
        """Analyze exit code to determine error type"""
        if exit_code == 0:
            return ErrorType.SUCCESS, "", None
        elif exit_code > 0:
            return (
                ErrorType.COMPILATION_ERROR,
                f"Compilation failed with exit code {exit_code}",
                None,
            )
        else:
            # Negative exit codes indicate signals
            signal_num = abs(exit_code)

            if signal_num == signal.SIGSEGV:
                return ErrorType.SEGFAULT, "Segmentation fault", signal_num
            elif signal_num == signal.SIGABRT:
                return (
                    ErrorType.MEMORY_ERROR,
                    "Aborted (likely assertion failure)",
                    signal_num,
                )
            elif signal_num == signal.SIGKILL:
                return ErrorType.TIMEOUT, "Killed (likely timeout)", signal_num
            elif signal_num == signal.SIGTERM:
                return ErrorType.TIMEOUT, "Terminated", signal_num
            else:
                return (
                    ErrorType.INVALID_EXIT_CODE,
                    f"Process terminated by signal {signal_num}",
                    signal_num,
                )

    def _create_error_result(
            self,
            target: BenchmarkTarget,
            file_path: Path,
            iteration: int,
            error_type: ErrorType,
            error_message: str,
    ) -> BenchmarkResult:
        """Create a BenchmarkResult for an error case"""
        return BenchmarkResult(
            target=target.value,
            file_path=str(file_path),
            compilation_time=0.0,
            execution_time=0.0,
            memory_peak_mb=0.0,
            executable_size_bytes=0,
            exit_code=-1,
            stdout="",
            stderr="",
            iteration=iteration,
            error_type=error_type,
            error_message=error_message,
            timed_out=False,
            signal_received=None,
        )

    def _measure_execution_time(
            self,
            runner: TargetRunner,
            file_path: Path,
            target: BenchmarkTarget,
            iteration: int,
    ) -> Tuple[float, ErrorType, str]:
        """Measure execution time using the provided runner and log output"""
        exec_cmd = runner.get_execution_command(file_path)
        if not exec_cmd:
            return 0.0, ErrorType.EXECUTION_ERROR, "No execution command available"

        try:
            start_time = time.perf_counter()
            log = self._get_log_file_path(target, file_path, iteration)

            # Start execution process with logging
            process = subprocess.Popen(
                exec_cmd,
                stdout=log.open('a'),
                stderr=subprocess.STDOUT,
                text=True,
                cwd=self.working_dir,
            )

            exit_code = process.wait(timeout=self.config.execution_timeout_seconds)
            end_time = time.perf_counter()

            if exit_code == 0:
                return end_time - start_time, ErrorType.SUCCESS, ""

            # Process was terminated by signal
            if exit_code < 0 and abs(exit_code) == signal.SIGSEGV:
                return (
                    0.0,
                    ErrorType.SEGFAULT,
                    f"{runner.get_target_name()} execution segfault",
                )

            return (
                0.0,
                ErrorType.EXECUTION_ERROR,
                f"{runner.get_target_name()} execution failed with exit code {exit_code}",
            )

        except subprocess.TimeoutExpired:
            return (
                0.0,
                ErrorType.TIMEOUT,
                f"{runner.get_target_name()} execution timed out after {self.config.execution_timeout_seconds}s",
            )
        except FileNotFoundError:
            return (
                0.0,
                ErrorType.EXECUTION_ERROR,
                f"{runner.get_target_name()} interpreter not found",
            )
        except Exception as e:
            return (
                0.0,
                ErrorType.EXECUTION_ERROR,
                f"{runner.get_target_name()} execution failed: {str(e)}",
            )

    def analyze_results(
            self, results: List[BenchmarkResult]
    ) -> Dict[str, BenchmarkStatistics]:
        """Perform comprehensive statistical analysis on successful results only"""
        if not results:
            return {}

        # Filter to only successful results for performance analysis
        successful_results = [r for r in results if r.error_type == ErrorType.SUCCESS]

        if not successful_results:
            return {}

        metrics = {
            "compilation_time":      [r.compilation_time for r in successful_results],
            "execution_time":        [
                r.execution_time for r in successful_results if r.execution_time > 0
            ],
            "memory_peak_mb":        [r.memory_peak_mb for r in successful_results],
            "executable_size_bytes": [
                r.executable_size_bytes
                for r in successful_results
                if r.executable_size_bytes > 0
            ],
        }

        statistics = {}
        for metric_name, values in metrics.items():
            if values:  # Only analyze if we have data
                statistics[metric_name] = self._calculate_statistics(values)

        return statistics

    def analyze_errors(self, results: List[BenchmarkResult]) -> Dict[str, int]:
        """Analyze error distribution in results"""
        error_counts = {}
        for result in results:
            error_type = result.error_type.value
            error_counts[error_type] = error_counts.get(error_type, 0) + 1

        return error_counts

    def _calculate_statistics(self, values: List[float]) -> BenchmarkStatistics:
        """Calculate comprehensive statistics for a metric"""
        values = np.array(values)

        if len(values) == 0:
            return BenchmarkStatistics(0, 0, 0, (0, 0), 0, 0, 0, 0, 0)

        # Remove outliers using IQR method if enabled
        clean_values = values
        outlier_count = 0

        if self.config.remove_outliers and len(values) >= 5:
            Q1 = np.percentile(values, 25)
            Q3 = np.percentile(values, 75)
            IQR = Q3 - Q1
            lower_bound = Q1 - 1.5 * IQR
            upper_bound = Q3 + 1.5 * IQR

            outlier_mask = (values < lower_bound) | (values > upper_bound)
            clean_values = values[~outlier_mask]
            outlier_count = int(np.sum(outlier_mask))

        if len(clean_values) == 0:
            clean_values = values  # Fallback if all values were outliers

        # Calculate statistics
        mean = np.mean(clean_values)
        median = np.median(clean_values)
        std_dev = np.std(clean_values, ddof=1) if len(clean_values) > 1 else 0

        # Calculate confidence interval
        if len(clean_values) >= 2:
            if len(clean_values) >= 30:
                # Use normal distribution for large samples
                ci_margin = stats.norm.ppf(
                    1 - (1 - self.config.confidence_level) / 2
                ) * (std_dev / np.sqrt(len(clean_values)))
            else:
                # Use t-distribution for small samples
                ci_margin = stats.t.ppf(
                    1 - (1 - self.config.confidence_level) / 2, len(clean_values) - 1
                ) * (std_dev / np.sqrt(len(clean_values)))

            confidence_interval = (mean - ci_margin, mean + ci_margin)
        else:
            confidence_interval = (mean, mean)

        return BenchmarkStatistics(
            mean=mean,
            median=median,
            std_dev=std_dev,
            confidence_interval=confidence_interval,
            coefficient_variation=(std_dev / mean * 100) if mean != 0 else 0,
            outlier_count=outlier_count,
            sample_size=len(clean_values),
            min_value=np.min(clean_values),
            max_value=np.max(clean_values),
        )

    def create_results_dataframe(self, results: List[BenchmarkResult]) -> pd.DataFrame:
        """Convert results to pandas DataFrame for analysis and export"""
        if not results:
            return pd.DataFrame()

        df_data = []
        for result in results:
            df_data.append(
                {
                    "target":                result.target,
                    "file":                  Path(result.file_path).name,
                    "iteration":             result.iteration,
                    "compilation_time_s":    result.compilation_time,
                    "execution_time_s":      result.execution_time,
                    "memory_peak_mb":        result.memory_peak_mb,
                    "executable_size_bytes": result.executable_size_bytes,
                    "exit_code":             result.exit_code,
                    "error_type":            result.error_type.value,
                    "error_message":         result.error_message,
                    "timed_out":             result.timed_out,
                    "signal_received":       result.signal_received,
                    "success":               result.error_type == ErrorType.SUCCESS,
                    "timestamp":             pd.Timestamp.now(),
                }
            )

        return pd.DataFrame(df_data)

    def create_summary_dataframe(
            self, statistics_by_target: Dict[str, Dict[str, BenchmarkStatistics]]
    ) -> pd.DataFrame:
        """Create summary DataFrame with statistics for LaTeX export"""
        summary_data = []

        for target, stats_dict in statistics_by_target.items():
            for metric, stats in stats_dict.items():
                summary_data.append(
                    {
                        "target":      target,
                        "metric":      metric,
                        "mean":        stats.mean,
                        "median":      stats.median,
                        "std_dev":     stats.std_dev,
                        "ci_lower":    stats.confidence_interval[0],
                        "ci_upper":    stats.confidence_interval[1],
                        "cv_percent":  stats.coefficient_variation,
                        "sample_size": stats.sample_size,
                        "outliers":    stats.outlier_count,
                        "min_value":   stats.min_value,
                        "max_value":   stats.max_value,
                    }
                )

        return pd.DataFrame(summary_data)

    def generate_report(
            self,
            results: List[BenchmarkResult],
            statistics_by_target: Dict[str, Dict[str, BenchmarkStatistics]],
            output_dir: Path = None,
    ) -> Path:
        """Generate comprehensive benchmark report with tables and graphs"""
        if output_dir is None:
            output_dir = Path("benchmark_results")

        output_dir.mkdir(exist_ok=True)

        # Create DataFrames
        results_df = self.create_results_dataframe(results)
        summary_df = self.create_summary_dataframe(statistics_by_target)

        # Analyze errors across all results
        all_error_analysis = {}
        for target_name in results_df["target"].unique():
            target_results = [r for r in results if r.target == target_name]
            all_error_analysis[target_name] = self.analyze_errors(target_results)

        # Save raw data (top level alongside LaTeX)
        results_df.to_csv(output_dir / "benchmark_raw_results.csv", index=False)
        summary_df.to_csv(output_dir / "benchmark_summary_statistics.csv", index=False)

        # Save error analysis (top level alongside LaTeX)
        error_df = pd.DataFrame.from_dict(all_error_analysis, orient="index").fillna(0)
        error_df.to_csv(output_dir / "benchmark_error_analysis.csv")

        # Generate structured visualizations
        self._create_structured_plots(results_df, output_dir)

        # Generate LaTeX tables
        results_df.to_latex(output_dir / "benchmark_raw_results.tex", index=False)

        # Generate text report
        report_path = self._generate_text_report(
            results_df, summary_df, statistics_by_target, all_error_analysis, output_dir
        )

        return report_path

    def _create_structured_plots(self, df: pd.DataFrame, output_dir: Path):
        """Create comprehensive structured plots for individual files and summaries"""
        if df.empty:
            return

        # Setup plot environment
        self._setup_plot_style()
        plot_dirs = self._create_plot_directories(output_dir)

        # Generate all plot types
        self._create_individual_file_plots(df, plot_dirs["individual"])
        self._create_summary_plots(df, plot_dirs["summary"])
        self._create_composite_plots(df, plot_dirs["summary"])
        self._create_legacy_plots(df, output_dir)

    def _setup_plot_style(self):
        """Configure matplotlib/seaborn for publication-quality plots"""
        plt.style.use("seaborn-v0_8")
        sns.set_palette("husl")
        plt.rcParams.update(
            {
                "figure.dpi":      300,
                "savefig.bbox":    "tight",
                "font.size":       10,
                "axes.labelsize":  10,
                "xtick.labelsize": 9,
                "ytick.labelsize": 9,
            }
        )

    def _create_plot_directories(self, output_dir: Path) -> Dict[str, Path]:
        """Create and return plot directory structure"""
        plots_dir = output_dir / "plots"
        individual_dir = plots_dir / "individual"
        summary_dir = plots_dir / "summary"

        for directory in [plots_dir, individual_dir, summary_dir]:
            directory.mkdir(exist_ok=True)

        return {
            "plots":      plots_dir,
            "individual": individual_dir,
            "summary":    summary_dir,
        }

    def _create_individual_file_plots(self, df: pd.DataFrame, output_dir: Path):
        """Create performance plots for each individual benchmark file"""
        success_df = df[df["success"]]

        for file_name in df["file"].unique():
            file_df = success_df[success_df["file"] == file_name]
            if file_df.empty:
                continue

            # Create file-specific directory
            file_dir = output_dir / file_name.replace(".bd", "")
            file_dir.mkdir(exist_ok=True)

            # Performance metrics plot
            fig, axes = plt.subplots(2, 2, figsize=(12, 10))
            fig.suptitle(
                f"Performance Results: {file_name}", fontsize=16, fontweight="bold"
            )

            # Compilation time
            if "compilation_time_s" in file_df.columns and len(file_df) > 1:
                if len(file_df["target"].unique()) > 1:
                    sns.boxplot(
                        data=file_df, x="target", y="compilation_time_s", ax=axes[0, 0]
                    )
                else:
                    file_df["compilation_time_s"].plot(
                        kind="hist", ax=axes[0, 0], bins=10, alpha=0.7
                    )
                axes[0, 0].set_title("Compilation Time")
                axes[0, 0].set_ylabel("Time (seconds)")

            # Execution time
            exec_df = file_df[file_df["execution_time_s"] > 0]
            if not exec_df.empty and len(exec_df) > 1:
                if len(exec_df["target"].unique()) > 1:
                    sns.boxplot(
                        data=exec_df, x="target", y="execution_time_s", ax=axes[0, 1]
                    )
                else:
                    exec_df["execution_time_s"].plot(
                        kind="hist", ax=axes[0, 1], bins=10, alpha=0.7
                    )
                axes[0, 1].set_title("Execution Time")
                axes[0, 1].set_ylabel("Time (seconds)")

            # Memory usage
            if "memory_peak_mb" in file_df.columns and len(file_df) > 1:
                if len(file_df["target"].unique()) > 1:
                    sns.boxplot(
                        data=file_df, x="target", y="memory_peak_mb", ax=axes[1, 0]
                    )
                else:
                    file_df["memory_peak_mb"].plot(
                        kind="hist", ax=axes[1, 0], bins=10, alpha=0.7
                    )
                axes[1, 0].set_title("Peak Memory Usage")
                axes[1, 0].set_ylabel("Memory (MB)")

            # Time series consistency
            if "iteration" in file_df.columns and len(file_df) > 5:
                for target in file_df["target"].unique():
                    target_df = file_df[file_df["target"] == target]
                    axes[1, 1].plot(
                        target_df["iteration"],
                        target_df["compilation_time_s"],
                        marker="o",
                        label=f"{target}",
                        alpha=0.7,
                        markersize=3,
                    )
                axes[1, 1].set_title("Performance Consistency")
                axes[1, 1].set_xlabel("Iteration")
                axes[1, 1].set_ylabel("Compilation Time (s)")
                axes[1, 1].legend()
                axes[1, 1].grid(True, alpha=0.3)

            plt.tight_layout()
            plt.savefig(file_dir / "performance.png", dpi=300, bbox_inches="tight")
            plt.savefig(file_dir / "performance.pdf", bbox_inches="tight")
            plt.close()

            # Error analysis for this file
            file_error_df = df[df["file"] == file_name]
            if not file_error_df.empty:
                self._create_file_error_plot(file_error_df, file_dir, file_name)

    def _create_file_error_plot(
            self, file_df: pd.DataFrame, output_dir: Path, file_name: str
    ):
        """Create error analysis plot for a specific file"""
        fig, axes = plt.subplots(1, 2, figsize=(12, 5))
        fig.suptitle(f"Error Analysis: {file_name}", fontsize=14, fontweight="bold")

        # Error distribution
        error_counts = file_df["error_type"].value_counts()
        if len(error_counts) > 1:
            axes[0].pie(
                error_counts.values, labels=error_counts.index, autopct="%1.1f%%"
            )
            axes[0].set_title("Error Distribution")
        else:
            axes[0].text(
                0.5,
                0.5,
                f"All runs: {error_counts.index[0]}",
                ha="center",
                va="center",
                transform=axes[0].transAxes,
                fontsize=12,
            )
            axes[0].set_title("Error Status")

        # Success rate by target
        success_rates = file_df.groupby("target")["success"].mean() * 100
        success_rates.plot(
            kind="bar",
            ax=axes[1],
            color=[
                "green" if x >= 90 else "orange" if x >= 70 else "red"
                for x in success_rates
            ],
        )
        axes[1].set_title("Success Rate by Target")
        axes[1].set_ylabel("Success Rate (%)")
        axes[1].set_xlabel("Target")
        axes[1].tick_params(axis="x", rotation=45)
        axes[1].set_ylim(0, 100)

        plt.tight_layout()
        plt.savefig(output_dir / "errors.png", dpi=300, bbox_inches="tight")
        plt.close()

    def _create_summary_plots(self, df: pd.DataFrame, output_dir: Path):
        """Create summary plots combining all benchmark files"""
        success_df = df[df["success"]]

        # Performance comparison across all files
        fig, axes = plt.subplots(2, 2, figsize=(16, 12))
        fig.suptitle(
            "BRAD Benchmark Suite - Summary Results", fontsize=18, fontweight="bold"
        )

        # Compilation time by file and target
        if not success_df.empty:
            sns.boxplot(
                data=success_df,
                x="file",
                y="compilation_time_s",
                hue="target",
                ax=axes[0, 0],
            )
            axes[0, 0].set_title("Compilation Time by File")
            axes[0, 0].set_ylabel("Time (seconds)")
            axes[0, 0].tick_params(axis="x", rotation=45)

            # Execution time by file (only for files with execution data)
            exec_df = success_df[success_df["execution_time_s"] > 0]
            if not exec_df.empty:
                sns.boxplot(
                    data=exec_df,
                    x="file",
                    y="execution_time_s",
                    hue="target",
                    ax=axes[0, 1],
                )
                axes[0, 1].set_title("Execution Time by File")
                axes[0, 1].set_ylabel("Time (seconds)")
                axes[0, 1].tick_params(axis="x", rotation=45)

            # Memory usage by file
            sns.boxplot(
                data=success_df,
                x="file",
                y="memory_peak_mb",
                hue="target",
                ax=axes[1, 0],
            )
            axes[1, 0].set_title("Memory Usage by File")
            axes[1, 0].set_ylabel("Memory (MB)")
            axes[1, 0].tick_params(axis="x", rotation=45)

            # Overall performance comparison
            summary_stats = (
                success_df.groupby(["file", "target"])["compilation_time_s"]
                .mean()
                .reset_index()
            )
            pivot_data = summary_stats.pivot(
                index="file", columns="target", values="compilation_time_s"
            )
            sns.heatmap(
                pivot_data, annot=True, fmt=".3f", cmap="viridis", ax=axes[1, 1]
            )
            axes[1, 1].set_title("Mean Compilation Time Heatmap")
            axes[1, 1].set_ylabel("File")

        plt.tight_layout()
        plt.savefig(
            output_dir / "summary_performance.png", dpi=300, bbox_inches="tight"
        )
        plt.savefig(output_dir / "summary_performance.pdf", bbox_inches="tight")
        plt.close()

        # Error analysis summary
        self._create_summary_error_plot(df, output_dir)

    def _create_summary_error_plot(self, df: pd.DataFrame, output_dir: Path):
        """Create summary error analysis plot"""
        fig, axes = plt.subplots(1, 3, figsize=(18, 6))
        fig.suptitle("Error Analysis Summary", fontsize=16, fontweight="bold")

        # Overall error distribution
        error_counts = df["error_type"].value_counts()
        axes[0].pie(error_counts.values, labels=error_counts.index, autopct="%1.1f%%")
        axes[0].set_title("Overall Error Distribution")

        # Success rate by file
        file_success = df.groupby("file")["success"].mean() * 100
        colors = [
            "green" if x >= 90 else "orange" if x >= 70 else "red" for x in file_success
        ]
        file_success.plot(kind="bar", ax=axes[1], color=colors)
        axes[1].set_title("Success Rate by File")
        axes[1].set_ylabel("Success Rate (%)")
        axes[1].tick_params(axis="x", rotation=45)
        axes[1].set_ylim(0, 100)

        # Success rate by target
        target_success = df.groupby("target")["success"].mean() * 100
        colors = [
            "green" if x >= 90 else "orange" if x >= 70 else "red"
            for x in target_success
        ]
        target_success.plot(kind="bar", ax=axes[2], color=colors)
        axes[2].set_title("Success Rate by Target")
        axes[2].set_ylabel("Success Rate (%)")
        axes[2].tick_params(axis="x", rotation=45)
        axes[2].set_ylim(0, 100)

        plt.tight_layout()
        plt.savefig(output_dir / "summary_errors.png", dpi=300, bbox_inches="tight")
        plt.close()

    def _create_composite_plots(self, df: pd.DataFrame, output_dir: Path):
        """Create comprehensive composite plots with multiple subplots"""
        success_df = df[df["success"]]

        # Large composite figure with multiple analysis views
        fig = plt.figure(figsize=(20, 16))
        gs = fig.add_gridspec(4, 4, hspace=0.4, wspace=0.3)

        fig.suptitle(
            "BRAD Language Benchmark Suite - Comprehensive Analysis",
            fontsize=20,
            fontweight="bold",
        )

        # Performance metrics (top row)
        ax1 = fig.add_subplot(gs[0, :2])
        if not success_df.empty:
            sns.boxplot(data=success_df, x="target", y="compilation_time_s", ax=ax1)
            ax1.set_title("Compilation Time Distribution")
            ax1.set_ylabel("Time (seconds)")

        ax2 = fig.add_subplot(gs[0, 2:])
        exec_df = success_df[success_df["execution_time_s"] > 0]
        if not exec_df.empty:
            sns.boxplot(data=exec_df, x="target", y="execution_time_s", ax=ax2)
            ax2.set_title("Execution Time Distribution")
            ax2.set_ylabel("Time (seconds)")

        # File comparison (second row)
        ax3 = fig.add_subplot(gs[1, :])
        if not success_df.empty:
            sns.violinplot(
                data=success_df, x="file", y="compilation_time_s", hue="target", ax=ax3
            )
            ax3.set_title("Compilation Time Distribution by File")
            ax3.tick_params(axis="x", rotation=45)

        # Memory analysis (third row)
        ax4 = fig.add_subplot(gs[2, :2])
        if not success_df.empty:
            sns.scatterplot(
                data=success_df,
                x="compilation_time_s",
                y="memory_peak_mb",
                hue="target",
                style="file",
                ax=ax4,
                alpha=0.7,
            )
            ax4.set_title("Compilation Time vs Memory Usage")
            ax4.set_xlabel("Compilation Time (s)")
            ax4.set_ylabel("Memory (MB)")

        ax5 = fig.add_subplot(gs[2, 2:])
        if not success_df.empty:
            memory_stats = (
                success_df.groupby(["file", "target"])["memory_peak_mb"]
                .mean()
                .reset_index()
            )
            pivot_memory = memory_stats.pivot(
                index="file", columns="target", values="memory_peak_mb"
            )
            sns.heatmap(pivot_memory, annot=True, fmt=".1f", cmap="plasma", ax=ax5)
            ax5.set_title("Mean Memory Usage Heatmap (MB)")

        # Error analysis (bottom row)
        ax6 = fig.add_subplot(gs[3, 0])
        error_counts = df["error_type"].value_counts()
        ax6.pie(error_counts.values, labels=error_counts.index, autopct="%1.1f%%")
        ax6.set_title("Error Distribution")

        ax7 = fig.add_subplot(gs[3, 1])
        target_success = df.groupby("target")["success"].mean() * 100
        colors = [
            "green" if x >= 90 else "orange" if x >= 70 else "red"
            for x in target_success
        ]
        target_success.plot(kind="bar", ax=ax7, color=colors)
        ax7.set_title("Success Rate by Target")
        ax7.set_ylabel("Success Rate (%)")
        ax7.tick_params(axis="x", rotation=45)
        ax7.set_ylim(0, 100)

        ax8 = fig.add_subplot(gs[3, 2:])
        if not success_df.empty and len(success_df) > 10:
            # Performance consistency over iterations
            for target in success_df["target"].unique():
                target_df = success_df[success_df["target"] == target]
                if len(target_df) > 5:
                    # Sample data if too many points
                    if len(target_df) > 50:
                        target_df = target_df.sample(50)
                    ax8.scatter(
                        target_df["iteration"],
                        target_df["compilation_time_s"],
                        label=target,
                        alpha=0.6,
                        s=20,
                    )
            ax8.set_title("Performance Consistency")
            ax8.set_xlabel("Iteration")
            ax8.set_ylabel("Compilation Time (s)")
            ax8.legend()
            ax8.grid(True, alpha=0.3)

        plt.savefig(
            output_dir / "comprehensive_analysis.png", dpi=300, bbox_inches="tight"
        )
        plt.savefig(output_dir / "comprehensive_analysis.pdf", bbox_inches="tight")
        plt.close()

    def _create_legacy_plots(self, df: pd.DataFrame, output_dir: Path):
        """Create original plots for backward compatibility"""
        success_df = df[df["success"]]

        if success_df.empty:
            return

        # Original performance comparison plot
        fig, axes = plt.subplots(2, 2, figsize=(12, 10))
        fig.suptitle(
            "BRAD Language Benchmark Results (Successful Runs)",
            fontsize=16,
            fontweight="bold",
        )

        # Compilation time boxplot
        if "compilation_time_s" in success_df.columns:
            sns.boxplot(
                data=success_df, x="target", y="compilation_time_s", ax=axes[0, 0]
            )
            axes[0, 0].set_title("Compilation Time Distribution")
            axes[0, 0].set_ylabel("Time (seconds)")

        # Execution time boxplot
        if (
                "execution_time_s" in success_df.columns
                and success_df["execution_time_s"].max() > 0
        ):
            execution_df = success_df[success_df["execution_time_s"] > 0]
            if not execution_df.empty:
                sns.boxplot(
                    data=execution_df, x="target", y="execution_time_s", ax=axes[0, 1]
                )
                axes[0, 1].set_title("Execution Time Distribution")
                axes[0, 1].set_ylabel("Time (seconds)")

        # Memory usage boxplot
        if "memory_peak_mb" in success_df.columns:
            sns.boxplot(data=success_df, x="target", y="memory_peak_mb", ax=axes[1, 0])
            axes[1, 0].set_title("Peak Memory Usage")
            axes[1, 0].set_ylabel("Memory (MB)")

        # Executable size comparison
        if (
                "executable_size_bytes" in success_df.columns
                and success_df["executable_size_bytes"].max() > 0
        ):
            size_df = success_df[success_df["executable_size_bytes"] > 0]
            if not size_df.empty:
                sns.boxplot(
                    data=size_df, x="target", y="executable_size_bytes", ax=axes[1, 1]
                )
                axes[1, 1].set_title("Executable Size")
                axes[1, 1].set_ylabel("Size (bytes)")

        plt.tight_layout()
        plt.savefig(
            output_dir / "performance_comparison.png", dpi=300, bbox_inches="tight"
        )
        plt.close()

        # Original error analysis plot
        fig, axes = plt.subplots(1, 2, figsize=(14, 6))
        fig.suptitle(
            "BRAD Language Benchmark Error Analysis", fontsize=16, fontweight="bold"
        )

        # Error count by type
        error_counts = df["error_type"].value_counts()
        if not error_counts.empty:
            axes[0].pie(
                error_counts.values, labels=error_counts.index, autopct="%1.1f%%"
            )
            axes[0].set_title("Overall Error Distribution")

        # Success rate by target
        if "target" in df.columns:
            success_rates = df.groupby("target")["success"].mean() * 100
            success_rates.plot(kind="bar", ax=axes[1], color="skyblue")
            axes[1].set_title("Success Rate by Target")
            axes[1].set_ylabel("Success Rate (%)")
            axes[1].set_xlabel("Target")
            axes[1].tick_params(axis="x", rotation=45)

        plt.tight_layout()
        plt.savefig(output_dir / "error_analysis.png", dpi=300, bbox_inches="tight")
        plt.close()

    def _generate_text_report(
            self,
            results_df: pd.DataFrame,
            summary_df: pd.DataFrame,
            statistics_by_target: Dict[str, Dict[str, BenchmarkStatistics]],
            error_analysis: Dict[str, Dict[str, int]],
            output_dir: Path,
    ) -> Path:
        """Generate comprehensive text report"""
        report_path = output_dir / "benchmark_report.md"

        with open(report_path, "w") as f:
            f.write("# BRAD Language Benchmark Report\n\n")
            f.write(
                f"Generated: {pd.Timestamp.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n"
            )

            # Executive Summary
            f.write("## Executive Summary\n\n")
            if not results_df.empty:
                total_runs = len(results_df)
                successful_runs = len(results_df[results_df["success"]])
                f.write(f"- **Total benchmark runs**: {total_runs}\n")
                f.write(
                    f"- **Successful runs**: {successful_runs} ({successful_runs / total_runs * 100:.1f}%)\n"
                )
                f.write(
                    f"- **Targets tested**: {', '.join(results_df['target'].unique())}\n"
                )
                f.write(
                    f"- **Files benchmarked**: {', '.join(results_df['file'].unique())}\n\n"
                )

                # Error summary
                if successful_runs < total_runs:
                    failed_runs = total_runs - successful_runs
                    f.write(
                        f"⚠️ **{failed_runs} runs failed** - see Error Analysis section for details.\n\n"
                    )

            # Error Analysis
            f.write("## Error Analysis\n\n")
            if error_analysis:
                f.write("### Error Summary by Target\n\n")
                for target, errors in error_analysis.items():
                    total_target_runs = sum(errors.values())
                    success_count = errors.get("success", 0)
                    success_rate = (
                        (success_count / total_target_runs * 100)
                        if total_target_runs > 0
                        else 0
                    )

                    f.write(
                        f"**{target}**: {success_count}/{total_target_runs} successful ({success_rate:.1f}%)\n"
                    )

                    # List error types
                    for error_type, count in errors.items():
                        if error_type != "success" and count > 0:
                            f.write(f"- {error_type}: {count}\n")
                    f.write("\n")

                # Common errors
                all_errors = {}
                for target_errors in error_analysis.values():
                    for error_type, count in target_errors.items():
                        if error_type != "success":
                            all_errors[error_type] = (
                                    all_errors.get(error_type, 0) + count
                            )

                if all_errors:
                    f.write("### Most Common Errors\n\n")
                    sorted_errors = sorted(
                        all_errors.items(), key=lambda x: x[1], reverse=True
                    )
                    for error_type, count in sorted_errors[:5]:
                        f.write(
                            f"1. **{error_type.replace('_', ' ').title()}**: {count} occurrences\n"
                        )
                    f.write("\n")

            # Statistical Summary (only for successful runs)
            f.write("## Statistical Summary (Successful Runs Only)\n\n")
            if not summary_df.empty:
                # Create LaTeX-ready table
                latex_table = summary_df.pivot_table(
                    index=["target", "metric"],
                    values=["mean", "std_dev", "ci_lower", "ci_upper"],
                    aggfunc="first",
                ).round(4)

                f.write("### Performance Statistics\n\n")
                f.write("```\n")
                f.write(
                    tabulate(latex_table, headers=latex_table.columns, tablefmt="grid")
                )
                f.write("\n```\n\n")
            else:
                f.write("No successful runs to analyze.\n\n")

            # Detailed Results by Target
            for target, stats_dict in statistics_by_target.items():
                f.write(f"## Results for {target.upper()}\n\n")

                for metric, stats in stats_dict.items():
                    f.write(f"### {metric.replace('_', ' ').title()}\n\n")
                    f.write(f"- **Mean**: {stats.mean:.6f}\n")
                    f.write(f"- **Median**: {stats.median:.6f}\n")
                    f.write(f"- **Standard Deviation**: {stats.std_dev:.6f}\n")
                    f.write(
                        f"- **95% Confidence Interval**: ({stats.confidence_interval[0]:.6f}, {stats.confidence_interval[1]:.6f})\n"
                    )
                    f.write(
                        f"- **Coefficient of Variation**: {stats.coefficient_variation:.2f}%\n"
                    )
                    f.write(f"- **Sample Size**: {stats.sample_size}\n")
                    f.write(f"- **Outliers Removed**: {stats.outlier_count}\n\n")

            # Methodology
            f.write("## Methodology\n\n")
            f.write(f"- **Iterations per target**: {self.config.iterations}\n")
            f.write(f"- **Warmup runs**: {self.config.warmup_runs}\n")
            f.write(f"- **Timeout**: {self.config.timeout_seconds} seconds\n")
            f.write(f"- **Confidence level**: {self.config.confidence_level * 100}%\n")
            f.write(
                f"- **Outlier removal**: {'Enabled' if self.config.remove_outliers else 'Disabled'}\n\n"
            )

            f.write("### BRAD Compilation Targets\n\n")
            f.write("1. **lua**: `cargo run v2 lua lib/std $FILE`\n")
            f.write(
                "   - Compiles BRAD to Lua and measures both compilation and execution time\n"
            )
            f.write("   - Measures executable size of generated `out.lua`\n\n")
            f.write(
                "2. **simple-spec-pipeline**: `cargo run v2 simple-spec-pipeline $FILE`\n"
            )
            f.write("   - Runs the simple specification pipeline\n")
            f.write("   - Measures compilation time and memory usage\n\n")

        return report_path


@click.command()
@click.argument(
    "paths",
    nargs=-1,
    required=True,
    type=click.Path(exists=True, path_type=Path),
    metavar="PATH...",
)
@click.option(
    "--targets",
    "-t",
    multiple=True,
    type=click.Choice([t.value for t in BenchmarkTarget], case_sensitive=False),
    default=[t.value for t in BenchmarkTarget],
    help="Compilation targets to test (can specify multiple)",
)
@click.option(
    "--iterations",
    "-i",
    default=DEFAULT_ITERATIONS,
    type=click.IntRange(5, 200),
    help="Number of benchmark iterations",
)
@click.option(
    "--warmup",
    "-w",
    default=DEFAULT_WARMUP_RUNS,
    type=click.IntRange(0, 10),
    help="Number of warmup runs",
)
@click.option(
    "--timeout",
    default=DEFAULT_TIMEOUT,
    type=click.IntRange(1, 3600),
    help="Compilation timeout in seconds",
)
@click.option(
    "--execution-timeout",
    default=DEFAULT_EXECUTION_TIMEOUT,
    type=click.IntRange(1, 300),
    help="Execution timeout in seconds",
)
@click.option(
    "--output",
    "-o",
    default=str(DEFAULT_OUTPUT_DIR),
    type=click.Path(path_type=Path),
    help="Output directory (default: bench/.results)",
)
@click.option(
    "--confidence",
    default=DEFAULT_CONFIDENCE,
    type=click.FloatRange(0.5, 0.99),
    help="Confidence level for statistical analysis",
)
@click.option("--no-outlier-removal", is_flag=True, help="Disable outlier removal")
@click.option(
    "--fail-fast",
    is_flag=True,
    help="Stop iterations within a file after consecutive failures (does not stop other files)",
)
@click.option(
    "--max-failures",
    default=5,
    type=click.IntRange(1, 20),
    help="Max consecutive failures before stopping",
)
@click.option(
    "--parallel",
    "-p",
    default=0,
    type=click.IntRange(0, 64),
    help="Number of parallel workers (0 = auto: cores/2, 1 = sequential)",
)
@click.option("--quiet", "-q", is_flag=True, help="Reduce output verbosity")
@click.version_option(version="1.0.0", prog_name="BRAD Benchmark Suite")
def main(
        paths,
        targets,
        iterations,
        warmup,
        timeout,
        execution_timeout,
        output,
        confidence,
        no_outlier_removal,
        fail_fast,
        max_failures,
        parallel,
        quiet,
):
    """
    🚀 BRAD Language Benchmark Suite

    A comprehensive benchmarking tool for the BRAD programming language.
    Supports statistical analysis, error detection, parallel execution, and LaTeX output.

    Examples:

      # Benchmark a single file
      benchmark.py tests/fib.bd

      # Benchmark entire directory (finds all .bd files)
      benchmark.py bench/simple/ --targets lua -p 8

      # Benchmark multiple directories and files
      benchmark.py bench/simple/ tests/fib.bd bench/plg/

      # Quick test with custom settings
      benchmark.py tests/ -i 10 -w 1 -p 1

      # Production benchmarking
      benchmark.py bench/ -p 16 --fail-fast -i 100 -o results/
    """

    # Discover all BRAD files from paths
    file_paths = discover_brad_files(list(paths))

    if not file_paths:
        click.secho("❌ No .bd files found in the specified paths", fg="red", err=True)
        sys.exit(1)

    if not quiet:
        file_count = click.style(str(len(file_paths)), fg="green", bold=True)
        click.echo(f"📁 Found {file_count} BRAD files to benchmark")

    # Create configuration
    config = BenchmarkConfig(
        iterations=iterations,
        warmup_runs=warmup,
        timeout_seconds=timeout,
        execution_timeout_seconds=execution_timeout,
        confidence_level=confidence,
        remove_outliers=not no_outlier_removal,
        fail_fast=fail_fast,
        max_consecutive_failures=max_failures,
        parallel_workers=parallel,
    )

    # Initialize runner
    try:
        runner = BradBenchmarkRunner(config)
        # Clean up old results first
        runner.cleanup_old_results(Path(output))
        # Setup logging directory for command output debugging
        runner.setup_logging(Path(output))
    except RuntimeError as e:
        click.secho(f"❌ {e}", fg="red", err=True)
        sys.exit(1)

    # Display startup info
    if not quiet:
        click.secho("\n🚀 BRAD Benchmark Suite", fg="cyan", bold=True)

        # Compact file and target info
        file_names = [f.name for f in file_paths]
        click.echo(f"   Files: {', '.join(file_names)}")
        click.echo(f"   Targets: {', '.join(targets)}")

        # Essential config info only
        total_runs = len(file_paths) * len(targets) * iterations
        click.echo(
            f"   {iterations} iterations × {len(file_paths)} files × {len(targets)} targets = {total_runs} total runs"
        )

        # Execution mode
        worker_count = config.get_worker_count()
        mode = (
            "Sequential" if worker_count == 1 else f"Parallel ({worker_count} workers)"
        )
        click.echo(f"   Mode: {mode}")

        # Timeouts and confidence
        click.echo(f"   Compilation timeout: {config.timeout_seconds}s")
        click.echo(f"   Execution timeout: {config.execution_timeout_seconds}s")

        click.echo()

    all_results = []
    statistics_by_target = {}

    failed_benchmarks = []

    try:
        # Simple execution loop
        for target_str in targets:
            target = BenchmarkTarget(target_str)
            target_results = []

            for file_path in file_paths:
                try:
                    results = runner.run_single_benchmark(target, file_path)
                    target_results.extend(results)
                    all_results.extend(results)
                except Exception as e:
                    # Log the failure but continue with other files
                    failed_benchmarks.append((target_str, file_path, str(e)))
                    click.secho(
                        f"❌ Failed: {file_path.name} with {target_str}: {e}", fg="red"
                    )
                    continue

            # Analyze results for this target
            if target_results:
                statistics_by_target[target.value] = runner.analyze_results(
                    target_results
                )

        # Generate report
        if not quiet:
            click.echo("📊 Generating report...")

        report_path = None
        try:
            report_path = runner.generate_report(
                all_results, statistics_by_target, output
            )

        except Exception as e:
            click.secho(f"\n⚠️  Could not generate full report: {e}", fg="yellow")
            # At least save the raw results if possible
            if all_results:
                try:
                    output.mkdir(exist_ok=True)
                    results_df = runner.create_results_dataframe(all_results)
                    results_df.to_csv(output / "benchmark_raw_results.csv", index=False)
                    click.secho(
                        f"   💾 Saved raw results to: {output}/benchmark_raw_results.csv",
                        fg="green",
                    )
                except Exception:
                    pass

        # Print summary
        completion_msg = (
            "✅ Benchmark completed!"
            if not failed_benchmarks
            else "⚠️  Benchmark completed with some failures!"
        )
        completion_color = "green" if not failed_benchmarks else "yellow"
        click.secho(f"\n{completion_msg}", fg=completion_color, bold=True)
        click.echo(f"   📁 Results saved to: {output}")

        # Show quick summary with error info
        total_runs = len(all_results)
        successful_runs = len(
            [r for r in all_results if r.error_type == ErrorType.SUCCESS]
        )

        click.secho("\n📈 Quick Summary:", fg="cyan", bold=True)
        success_rate = successful_runs / total_runs * 100 if total_runs > 0 else 0
        color = (
            "green" if success_rate >= 90 else "yellow" if success_rate >= 70 else "red"
        )
        click.secho(
            f"   Overall: {successful_runs}/{total_runs} successful ({success_rate:.1f}%)",
            fg=color,
        )

        # Report failed benchmarks
        if failed_benchmarks:
            click.secho(
                f"\n⚠️  Failed Benchmarks ({len(failed_benchmarks)}):",
                fg="yellow",
                bold=True,
            )
            for target, file_path, error in failed_benchmarks:
                click.echo(f"   {target} → {file_path.name}: {error}")

        if statistics_by_target:
            for target, stats in statistics_by_target.items():
                if "compilation_time" in stats:
                    comp_time = stats["compilation_time"]
                    click.echo(
                        f"   {target}: {comp_time.mean:.3f}s ± {comp_time.std_dev:.3f}s compilation"
                    )

    except KeyboardInterrupt as e:
        click.secho("\n⚠️  Benchmark interrupted by user", fg="yellow")

        # Dump traceback to see where we were
        if click.get_current_context().obj is None:
            import traceback

            traceback.print_exc()

        sys.exit(1)
    except Exception as e:
        click.secho(f"\n❌ Benchmark failed: {e}", fg="red", err=True)
        if click.get_current_context().obj is None:  # Not in testing
            import traceback

            traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
