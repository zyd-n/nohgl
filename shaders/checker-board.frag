#version 330 core

out vec4 FragColor;

in vec2 UV;
in vec3 FragPos;
in vec3 Normal;

uniform float r;
uniform float g;
uniform float b;

uniform vec3 object_color;
uniform vec3 light_color;
uniform vec3 light_pos;

void main()
{
  // Ambient
  float ambientStrength = 0.1;
  vec3 ambient = ambientStrength * light_color;

  // Diffuse
  vec3 norm = normalize(Normal);
  vec3 lightDir = normalize(light_pos - FragPos);
  float diff = max(dot(norm, lightDir), 0.0);
  //float diff = dot(norm, lightDir);
  vec3 diffuse = diff * light_color;

  vec3 result = (ambient + diffuse) * object_color;
  // FragColor = vec4(result, 1.0);
  FragColor = vec4(vec3(r, g, b) * result, 0.0f);
}
