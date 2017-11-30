/*
 * Water fragment shader.
 */
#version 330 core

// The interpolated clip space coordinate for the fragment.
in vec4 clipSpace;

// Texture coordinates for the DuDv map.
in vec2 dudvCoord;

// The water color.
uniform vec3 waterColor;

// The refraction texture.
uniform sampler2D refractionTexture;

// The reflection texture.
uniform sampler2D reflectionTexture;

// The DuDv texture.
uniform sampler2D dudvTexture;

// The resulting output color.
out vec4 color;

void main()
{
  // Make a texture coordinate/normalized device coordinate.
  vec2 texCoord = (clipSpace.xy / clipSpace.w) / 2 + 0.5;

  // Pick the DuDv distortion value.
  vec3 dudv = texture2D(dudvTexture, dudvCoord).rgb;

  // Pick the refraction color.
  vec3 refraction = texture2D(refractionTexture, texCoord).rgb;

  // Pick the reflection color.
  vec3 reflection = texture2D(reflectionTexture, vec2(texCoord.s, 1 - texCoord.t)).rgb;

  // Mix the water color with the refraction texture.
  //color = vec4(mix(refraction, reflection, 0.5), 1);
  color = vec4(dudv, 1);
}
