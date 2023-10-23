import numpy as np
from matplotlib import pyplot as plt
from PIL import ImageEnhance
from PIL import Image

# img_data = Image.open("BT-10-7-NC.jpeg")


def percent_cover(img, ROI=None, visualize=False):
    '''
    Gets the percent plant cover of a given ROI in an image

    Arguments
        img : PIL.Image
            The image to be processed
        ROI : tuple(x1), x2, y1, y2)
            where x, y are the limits of
            a given rectangular region of interest.
            i.e.:

            x1y1---------------------x2y1
             |                         |
             |                         |
             |                         |
             |                         |
             |                         |
             |                         |
            x1y2---------------------x2y2

    '''
    converter = ImageEnhance.Color(img)
    saturated = np.array(converter.enhance(30))
    if ROI != None:
        y1, y2, x1, x2 = ROI
        target = saturated[x1:x2, y1:y2]
    else:
        target = saturated
    greens = target[np.logical_and(target[:,:, 1] > target[:,:, 2], target[:,:, 1] > target[:,:, 0])]
    if visualize:
        fig, ax = plt.subplots(1,3)
        for a in ax:
            a.get_yaxis().set_visible(False)
            a.get_xaxis().set_visible(False)

        ax[1].imshow(saturated)
        ax[0].imshow(img)
        target[np.logical_or(target[:,:, 1] < target[:,:, 2], target[:,:, 1] < target[:,:, 0])] = [0, 0, 0]
        ax[2].imshow(target)
        plt.show()
    arr = np.array(img)
    if ROI is not None:
        return len(greens) / np.abs((x1 - x2) * (y1 - y2))
    else:
        # print(len(arr), len(arr[0]))
        return len(greens) / (len(arr) * len(arr[0]))


# print(percent_cover(img_data, (355, 936, 0, 1165)))
