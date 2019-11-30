## @package loon
from .loon_class import loon,loon_l_context,loon_l_glyph,loon_l_graph,loon_l_hist,loon_l_layer,loon_l_navigator,loon_l_pairs,loon_l_plot,loon_l_plot3D,loon_l_serialaxes,loon_l_compound
from .l_data import l_data
from .l_hist import l_hist
from .l_plot import l_plot
from .l_serialaxes import l_serialaxes
from .l_subwin import l_subwin
from .l_toplevel import l_toplevel
from .loonPlotFactory import loonPlotFactory
from .dataset import iris,olive,oliveAcids,oliveLocations,quakes,faithful
from .graphutils import loongraph,completegraph
from .l_graph import l_graph
from .l_plot3D import l_plot3D
from .l_isLoonWidget import l_isLoonWidget
from .l_throwErrorIfNotLoonWidget import l_throwErrorIfNotLoonWidget
from .l_cget import l_cget
from .l_widget import l_widget
from .l_configure import l_configure
from .l_info import l_info_states
from .l_state_names import l_state_names
from .l_scaleto import l_scaleto, l_scaleto_world,l_scaleto_plot,l_scaleto_selected,l_scaleto_active
from .l_layer import l_layer_add,l_layer_polygon,l_layer_polygons,l_layer_rectangle,l_layer_rectangles,l_layer_line,l_layer_lines,l_layer_oval,l_layer_text,l_layer_texts
from .l_ColorList import hex12tohex6,color_loon,loon_palette,l_colRemoveAlpha,l_setColorList,l_getColorList,l_setColorList_ColorBrewer,l_setColorList_hcl,l_setColorList_ggplot2,l_setColorList_baseR,l_setColorList_loon
from .l_hexcolor import l_hexcolor 
from .tkcolors import tkcolors
from .l_resize import l_resize
from .l_zoom import l_zoom
from .l_pairs import l_pairs,l_getPlots,l_getLocations
from .l_help import l_help
from .l_web import l_web
from .l_linkedStates import l_setLinkedStates,l_getLinkedStates
from .l_getOption import l_getOption,l_getOptionNames,l_userOptions,l_userOptionDefault,l_setOption
from .l_compound import l_getPlots,l_getLocations
from .l_move import l_move,l_move_grid,l_move_halign,l_move_hdist,l_move_jitter,l_move_reset,l_move_valign,l_move_vdist
from .l_redraw import l_redraw
from .l_size import l_size
from .l_export import l_export,filetypes,l_export_valid_formats,exportImageDialog
from .l_saveStates import l_saveStates
from .l_setTitleFont import l_setTitleFont
from .l_copyStates import l_copyStates
from .tk import tk

### remove later 
#from .loonobject import *
###
namespace = globals().keys()
__all__ = ['tk','l_data','l_hist','l_plot','l_serialaxes','l_subwin','l_toplevel',
            'loon','loon_l_context','loon_l_glyph','loon_l_graph','loon_l_hist',
            'loon_l_layer','loon_l_navigator','loon_l_pairs','loon_l_plot',
            'loon_l_plot3D','loon_l_serialaxes','loon_l_compound','loonPlotFactory','iris',
            'olive','oliveAcids','oliveLocations','quakes','faithful',
            'loongraph','completegraph','l_graph','l_plot3D','l_isLoonWidget',
            'l_throwErrorIfNotLoonWidget','l_cget','l_widget','l_configure',
            'l_info_states','l_state_names','l_scaleto', 'l_scaleto_world',
            'l_scaleto_plot','l_scaleto_selected','l_scaleto_active',
            'l_layer_add','l_layer_polygon','l_layer_polygons','l_layer_rectangle',
            'l_layer_rectangles','l_layer_line','l_layer_lines','l_layer_oval',
            'l_layer_text','l_layer_texts','hex12tohex6','color_loon','loon_palette',
            'l_colRemoveAlpha','l_setColorList','l_getColorList','l_setColorList_ColorBrewer',
            'l_setColorList_hcl','l_setColorList_ggplot2','l_setColorList_baseR',
            'l_setColorList_loon','l_hexcolor','tkcolors','l_resize','l_zoom',
            'l_pairs','l_help','l_web','l_setLinkedStates','l_getLinkedStates',
            'l_getOption','l_getOptionNames','l_userOptions','l_userOptionDefault','l_setOption',
            'l_getPlots','l_getLocations','l_move','l_move_grid','l_move_halign',
            'l_move_hdist','l_move_jitter','l_move_reset','l_move_valign','l_move_vdist',
            'l_size','l_export','l_export_valid_formats','exportImageDialog',
            'l_saveStates','l_setTitleFont','l_copyStates']