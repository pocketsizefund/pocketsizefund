import sys


original_name = sys.argv[1]

updated_name = ''
if original_name == 'fetchdata':
    updated_name = 'fetch-data'

elif original_name == 'createpositions':
    updated_name = 'create-positions'

elif original_name == 'clearpositions':
    updated_name = 'clear-positions'

elif original_name == 'completeinvite':
    updated_name = 'complete-invite'

elif original_name == 'lstm':
    updated_name = 'model-lstm'

else:
    exit('"{}" package name not found'.format(original_name))

print(updated_name)
