import os
import shutil

for x in ['hello-world', 'sliders', 'loop']:
  shutil.copy("packages.dhall", f'{x}/packages.dhall')
