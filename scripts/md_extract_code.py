#!/usr/bin/env python
"""Extract a code block from a Markdown file."""

import argparse

def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument("file", help="The file to parse")
    parser.add_argument("block_num", help="The index of the codeblock to extract (0-indexed)", type=int)
    parser.add_argument("-o", "--output", help="The file to output the code block to (defaults to stdout)")

    return parser.parse_args()

def extract_codeblock(md_file, block_num, output):
    code_blocks = get_codeblocks(md_file)

    try:
        code_block = code_blocks[block_num]
    except IndexError:
        raise IndexError("Block index {} out of bounds: markdown file has {} code block(s)".format(block_num, len(code_blocks)))

    if output is None:
        print(code_block)
    else:
        open(output, 'w').write(code_block)

def get_codeblocks(md_file):
    code_blocks = []
    curr_code_block = None
    for line in open(md_file, 'r').readlines():
        if line.startswith('```'):
            if curr_code_block is None:
                curr_code_block = []
            else:
                code_blocks.append(''.join(curr_code_block))
                curr_code_block = None
        elif curr_code_block is not None:
            curr_code_block.append(line)

    return code_blocks

if __name__ == "__main__":
    args = parse_args()
    extract_codeblock(args.file, args.block_num, args.output)
