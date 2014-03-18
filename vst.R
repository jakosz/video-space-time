library(jap)
library(igraph)
u = function(){source('vst.R')}

w = function(...) write(ps(...), stdout())

# "Round" timestamps to full minutes and spread the data frame to a regular timestamp sequence:
timeline <-
function(x, control_variable) {
    x = x[order(x$timestamp), ]
    if (any(is.na(x$timestamp))) {
        x = x[-which(is.na(x$timestamp)), ]
    }
    timestamp = seq(x$timestamp[1], x$timestamp[nrow(x)], by = 60)
    y = data.frame(matrix(0, nrow = length(timestamp), ncol = ncol(x)))
    names(y) = names(x)
    y$timestamp = timestamp
    y$datetime = strftime(as.POSIXlt(y$timestamp, origin = '1970-01-01'), format='%Y-%m-%d %H:%M')
    w('merging dataframes...')
    res = do.call('rbind', args = list(x, y))
    res = res[order(res$timestamp), ]
    w('encoding datetimes...')
    return(tuniq(res, control_variable))
}

# Removes repeating time entries produced by timeline() if control_variable == 0
tuniq <-
function(Timeline, control_variable) {
    reps = as.data.frame(table(Timeline$timestamp))
    reps = reps[which(reps$Freq > 1) , ]
    Timeline$check = rep(0, nrow(Timeline))
    Timeline$check[which(as.numeric(Timeline$timestamp) %in% as.numeric(as.character(reps$Var1)))] = 1
    out = which(Timeline$check == 1 & Timeline[[control_variable]] == 0)
    if (length(out) != 0) Timeline = Timeline[-out, ]
    return(Timeline)
}

# Generate half-circle layout for legends:
hcLayout <-
function(n, side) {
    l = layout.circle(graph.data.frame(data.frame(1:n*2, 1:n)))
    if (side == 1) {
        return(l[which(l[,2] <= 0), ])
    }
    if (side == 2) {
        return(l[which(l[,1] <= 0), ])
    }
    if (side == 3) {
        return(l[which(l[,2] >= 0), ])
    }
    if (side == 4) {
        return(l[which(l[,1] >= 0), ])
    }
}

# Adds zeroes to the string (for time formatting and easy later ffmpeg invocation)
zeroes <-
function(x, n) {
    zeros <-
        function(x, n) {
        diff = n - nchar(as.character(x))
        if (diff != 0) {
            return(ps(paste(rep('0', diff), collapse = ''), x))
        } else {
            return(x)
        }
    }
    return(unlist(lapply(x, zeros, n = n)))
}

# List constructor; returns a list of class 'vst'
vst_list <-
function(...) {
    res = list(...)
    class(res) = 'vst'
    add <- function(x, v, res) { if (!x %in% names(res)) { res[[x]] = v }; return(res) }
    res = add('mc.cores', detectCores(), res)
    res = add('col_bg', 'black', res)
    res = add('col_txt_1', rgb(1,1,1,.5), res)
    res = add('marLim', list(x=c(0,0),y=c(0,0)), res)
    res = add('font', 'Roboto', res)
    res = add('font_light', 'Roboto Light', res)
    res = add('fadeout_duration', 30, res)
    res = add('color_palette', heat.colors(length(unique(res$data[[res$color]]))), res)
    res = add('size_cex_init', 2, res)
    res = add('vst_layout', c(1, rep(2, 4), 3), res)
    return(res)
}

# Loads list elements as variables into parent environment
vst_unpack <-
function(list, parent = 1) {
    for (i in seq_along(list)) {
        assign(names(list)[i], list[[i]], envir = parent.frame(parent))
    }
}

# Generates frames for the video:
plot.vst <-
function(args) {
    vst_unpack(args)
    # Select rows containing event data
    cd = data[which(data[[size]] != 0), ]
    # Update args with data parameters
    nc = sort(unique(cd[[color]]))
    ncc = rev(heat.colors(length(nc)))
    nv = sort(unique(cd[[size]]))
    args$nc = nc
    args$ncc = ncc
    args$nv = nv
    # xlim, ylim
    yLim = c(min(cd[[latitude]]) + marLim$y[1], max(cd[[latitude]]) + marLim$y[2])
    xLim = c(min(cd[[longitude]]) + marLim$x[1], max(cd[[longitude]]) + marLim$x[2])
    args$xLim = xLim
    args$yLim = yLim
    mclapply(offset:(offset+n), mc.cores = mc.cores, function(i) {
    #lapply(offset:(offset+n), function(i) {
        w(i)
        jpeg(ps(dir, '/', file, '_', zeroes(i, nchar(offset+n)), '.jpg'), width = width, height = height)
        par(bg=col_bg, mar = rep(1, 4))
        # Set layout:
        layout(matrix(vst_layout, nrow = 1))
        # Size legend:
        vst_size_legend(args, i)
        # Plot all event points for context:
        plot(data[[longitude]], data[[latitude]], 
             pch = 19, cex = .1, col = 'grey30', ylim = yLim, xlim = xLim, 
             xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
            # Plot current event:
            if (data[[size]][i] != 0) {
                print(i)
                points(data[[longitude]][i], data[[latitude]][i], pch = 19, 
                       cex = data[[size]][i] * 2, 
                       col = rev(heat.colors(max(data[[color]])))[data[[color]][i]])
            }
            # Draw fading-out points:
            vst_fadeout(args, i)
            # Add title and current timestamp:
            mtext(title, side = 3, line = -7, cex = 3, col = rgb(1,1,1,.75), family = 'Roboto')
            mtext(data$datetime[i], side = 3, line = -13, cex = 4, col = col_txt_1, family = 'Roboto Light')
            # Plot color legend:
            vst_color_legend(args, i)
        dev.off()
    })
}

# Handles point fadeout in time
vst_fadeout <-
function(args, i) {
    vst_unpack(args)
    if (i <= fadeout_duration) {
        ccd = data[1:i, ]
    } else {
        ccd = data[(i-fadeout_duration):i, ]
    }
    Cex = ccd[[size]] / nrow(ccd):1 * 2
    if (length(Cex) == 1) { Cex = ccd[[size]] } else { Cex = sr(Cex, .5, max(Cex)) }
    points(ccd[[longitude]], ccd[[latitude]], pch = 19,
           col = rev(heat.colors(max(data[[color]])))[ccd[[color]]], 
           cex = Cex)
}

# Draws a legend for the dot color
vst_color_legend <-
function(args, i) {
    vst_unpack(args)
    # Params:
    cx = -.5
    cy = c(-.1, .1)
    # --
    plot_empty()
    points(rep(cx, length(nc)), sr(seq_along(nc), cy[1], cy[2]), cex = 1, pch = 15, col = rev(heat.colors(length(nc))))
    # color_text
    text(cx + .1, cy[2] + .1, labels = color_text, family = 'Roboto Light', col = col_txt_1, pos = 4, cex = 3)
    # color scale:
    text(cx - .05, cy[2], labels = max(nc), family = 'Roboto Light', col = ncc[length(ncc)], pos = 2, cex = 2)
    text(cx - .05, cy[1], labels = min(nc), family = 'Roboto Light', col = ncc[1], pos = 2, cex = 2)
    # Counter:
    text(0, 0, labels = sum(data[[color]][1:i]), family = 'Roboto Light', col = col_txt_1, pos = 4, cex = 7)
}

# Draws a legend for the dot size
vst_size_legend <-
function(args, i) {
    vst_unpack(args)
    # Params:
    cx = .5
    cy = c(-.1, .1)
    # --
    plot_empty()
    # Biggest point:
    points(cx, 0, pch = 19, cex = max(data[[size]]), col = col_txt_1)
    rect(xleft = -1, xright = cx, ybottom = -1, ytop = 1, col = col_bg)
    text(labels = max(nv), x = cx + .05, y = 0, family = 'Roboto Light', pos = 4, cex = 2, col = col_bg)
    # Smallest point:
    points(cx, 0, pch = 19, cex = min(data[[size]]), col = col_bg)
    text(labels = min(nv), x = cx - .05, y = 0, family = 'Roboto Light', pos = 2, cex = 2, col = col_txt_1)
    # size_text:
    text(cx - .1, cy[2] + .1, labels = size_text, family = 'Roboto Light', col = col_txt_1, pos = 2, cex = 3)
    # Counter:
    text(0, 0, labels = sum(data[[size]][1:i]), family = 'Roboto Light', col = col_txt_1, pos = 2, cex = 7)
}
