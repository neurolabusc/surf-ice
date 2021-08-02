import gl
import sys
print(sys.version)
print(sys.path)
print(gl.__doc__)
for key in dir( gl ):
  if not key.startswith('_'):
    x = getattr( gl, key ).__doc__
    print(key+' (built-in function): ')
    print(x)