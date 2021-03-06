from .l_isLoonWidget import *
from sys import exit
def l_throwErrorIfNotLoonWidget(widget):
    """Throw an error if string is not associated with a loon widget
    
    Helper function to ensure that a widget path is associated with a
    loon widget.

    Args:
        widget: widget path name as a string

    Returns:
        True if the string is associated with a loon widget,
        otherwise an error is thrown.       
    """
    if(not l_isLoonWidget(widget)):
        if(not isinstance(widget, str)):
            widget = widget.plot 
        exit('Widget path "' + widget + '" is not a known loon widget.')
    return True
