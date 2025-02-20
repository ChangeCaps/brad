import re
import subprocess

import click

@click.group()
def main():
    pass

def output_bnf_block(lines):
        if lines[:-1] == ";;":
            lines = lines[:-1]

        x = """\\begin{bnf}[
    colspec = {llcll},
    column{1} = {font = \\sffamily},
    column{2} = {font = \\ttfamily},
    column{3} = {font = \\ttfamily},
    column{4} = {font = \\ttfamily},
]
%s
\\end{bnf}""" % "\n".join(lines)
        x = x.replace('_', '\\_')
        return x
    

@main.command()
@click.argument('input_file')
def fix(input_file):
    content = open(input_file).readlines()
    output = []

    for line in content:  
        line = line.rstrip()

        # Skip comments
        if line.startswith('//') or line.startswith('/*'):
            continue

        # if // is at the end of the line, remove it
        if '//' in line:
            line = line.split('//')[0].rstrip()

        if not line or line.strip().startswith('<'): # new rule
            # Ensure last output is GAP
            if output and output[-1] != ";;":
                output.append(";;")

            # If length of output is greater than 50 lines, output the block and reset
            if len(output) + output.count(";;") > 50:
                print(output_bnf_block(output))
                output = []

        if not line:
            continue

        # escape 
        escape_chars = ['_', '%', '{', '}', '&', '$', '#', '^', '~', '\\']
        #for char in escape_chars:
        #    line = line.replace(char, '\\' + char)

        # replace <\w+> with \bnfvar{\1}
        line = re.sub(r'<(\w+)>', r'\\bnfv{\1}', line)

        
        output.append(line)

    
    print(output_bnf_block(output))


if __name__ == '__main__':
    main()