/*
 * Water fragment shader.
 */
 #version 330 core

 uniform vec3 waterColor;
 uniform sampler2D refractionTexture;

 out vec4 color;

 void main()
 {
   color = vec4(waterColor, 1);
 }
