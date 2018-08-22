
import os
os.chdir('Y:/PiotrT/RNA-FISH/scripts/merging')
cwd = os.getcwd()
# print(cwd)

from skimage import io
# from skimage.external import tifffile as tif
# from osgeo import gdal
import numpy as np
# from osgeo import gdal_array

# im1=gdal.Open('A488_0-9_Z-stack_fish.tif')

# im2=gdal.Open('DAPI_0-9_Z-stack_fish.tif')
print(2)
# im3=np.stack((im1, im2), axis=0)
# import numpy as np

im1 = io.imread('A488_0-9_Z-stack_fish.tif')
im2 = io.imread('DAPI_0-9_Z-stack_fish.tif')
# im2 = io.imread('new.tif')
# print(im2.shape)
# im3=np.stack((im1, im2, im1), axis=0)
# print(im1.shape)
imlist=[im1,im2]
con_im=io.concatenate_images(imlist)
# print(con_im.shape)

# tif.imsave('new.tif', con_im)
io.imsave('new.tiff', im1)
