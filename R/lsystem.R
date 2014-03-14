# Lsystems

#
# F: Move forward (and draw a line)
# +: Rotate left
# -: Rotate right
# [: Push current state onto a stack
# ]: Pop stack and replace current state
#

p <- c(5, 0)
plot(5, 0, xlim = c(0, 10), ylim = c(0, 10))



segments(5, 0, 5, 1)


segments(5, 1, 5-cos(45), 1+sin(45))
