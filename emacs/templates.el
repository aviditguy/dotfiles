;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my--c-template
  "// :flags -lm
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <time.h>

int main(void)
{
  %s
  return 0;
}")

(defvar my--raylib-template
  "// :flags -lraylib -lm
#include <raylib.h>
#include <math.h>

int main(void)
{
    InitWindow(800, 600, \"Raylib\");
    SetTargetFPS(60);

    while (!WindowShouldClose())
    {
        BeginDrawing();
        ClearBackground(RAYWHITE);
	%s
        EndDrawing();
    }

    CloseWindow();
}")

(defvar my--canvas-template
  "<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\"
          content=\"width=device-width, initial-scale=1.0\">

    <style>
      *{
	  margin: 0;
	  padding: 0;
	  box-sizing: border-box;
      }
      
      canvas {
	  border: 1px solid gray;
      }
    </style>

  </head>
  <body>
    <canvas id=\"canvas1\"></canvas>

    <script>
      const canvas = document.getElementById(\"canvas1\");
      const ctx    = canvas.getContext(\"2d\");

      canvas.width  = 600;
      canvas.height = 400;

      function update()
      {
      }

      function draw()
      {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
      }
      
      function loop()
      {
        update();
        draw();
        requestAnimationFrame(loop);
      }

      %s

    </script>
  </body>
</html>")


(defun my-template-insert (template)
  (insert (format template "")))

(defun @c-template ()
  (interactive)
  (my-template-insert my--c-template))

(defun @raylib-template ()
  (interactive)
  (my-template-insert my--raylib-template))

(defun @canvas-template ()
  (interactive)
  (my-template-insert my--canvas-template))


