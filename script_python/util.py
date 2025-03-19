# Utility functions

import os
from posixpath import splitext

AUDIO_EXTS = [
  '.3gp', '.3gpp', '.8svx', '.aa', '.aac', '.aax', '.act', '.aif', '.aiff', '.alac', '.amr', '.ape', '.au',
  '.awb', '.cda', '.dss', '.dvf', '.flac', '.gsm', '.iklax', '.ivs', '.m4a', '.m4b', '.m4p', '.mmf',
  '.mp3', '.mpc', '.mpga', '.msv', '.nmf', '.octet-stream', '.ogg', '.oga', '.mogg', '.opus', '.org',
  '.ra', '.rm', '.raw', '.rf64', '.sln', '.tta', '.voc', '.vox', '.wav', '.wma', '.wv', '.webm', '.x-m4a',
]

# return list of audio files in the given directory;
# returned file names are fully qualified paths, unless short_names=True
def get_audio_files(path, short_names=False):
    files = []
    if os.path.isdir(path):
        for file_name in sorted(os.listdir(path)):
            file_path = os.path.join(path, file_name)
            if os.path.isfile(file_path):
                base, ext = os.path.splitext(file_path)
                if ext != None and len(ext) > 0 and ext.lower() in AUDIO_EXTS:
                    if short_names:
                        files.append(file_name)
                    else:
                        files.append(file_path)

    return sorted(files)

# return list of strings representing the lines in a text file,
# removing leading and trailing whitespace and ignoring blank lines
# and lines that start with #
def get_file_lines(path):
    try:
        with open(path, 'r') as file:
            lines = []
            for line in file.readlines():
                line = line.strip()
                if len(line) > 0 and line[0] != '#':
                    lines.append(line)

            return lines
    except IOError:
        print(f'Unable to open input file {path}')
        return []

# return a dictionary mapping class names to banding codes, based on the classes file;
# if reverse=True, map codes to class names
def get_class_dict(class_file_path, reverse=False):
    lines = get_file_lines(class_file_path)
    class_dict = {}
    for line in lines:
        tokens = line.split(',')
        if len(tokens) == 2:
            if reverse:
                class_dict[tokens[1]] = tokens[0]
            else:
                class_dict[tokens[0]] = tokens[1]

    return class_dict
