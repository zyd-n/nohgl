#version 330 core

layout (location = 0) in vec3 iPosition;
layout (location = 1) in vec3 iNormal;
layout (location = 2) in vec2 iUV;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 FragPos;
out vec3 Normal;

void main()
{
  FragPos = vec3(model * vec4(iPosition, 1.0));
  Normal = mat3(transpose(inverse(model))) * iNormal;
  // UV = vec2(iUV.x, iUV.y);

  gl_Position = projection * view * vec4(iPosition, 1.0);
}
