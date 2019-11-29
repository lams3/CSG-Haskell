#version 330 core

layout(location = 0) in vec3 vPosition_modelspace;

void main()
{
    gl_Position.xyz = vPosition_modelspace;
    gl_Position.w = 1.0;
}