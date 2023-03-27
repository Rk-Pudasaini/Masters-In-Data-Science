#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#define MAXSIZE 10       
int stack[MAXSIZE];     
int top = -1;
int isempty() 
{
	if(top == -1)
    	return 1;
   	else
		return 0;
}
int isfull() 
{
	if(top == MAXSIZE - 1)
    	return 1;
   	else
    	return 0;
}
int peek() 
{
	if(isempty()) 
   		 printf("Stack is empty.\n");  
   	else 
   		 return stack[top]; 	
}

void push(int data) 
{
	if(isfull()) 
		printf("Stack is full.\n");
   	else
	
		stack[++top] = data;
}
int pop() 
{
	if(isempty()) 
		printf("Stack is empty.\n");
   	else	
		return stack[top--];
}
int match(char a, char b)
{
	if(a == '[' && b == ']')
    	return 1;
    if(a == '{' && b == '}')
    	return 1;
    if(a == '(' && b == ')')
    	return 1;
    return 0;
}
void bracketmatch(char exp[])
{
    char temp;
    for(int i=0; i < strlen(exp); i++)
    {
    	if(exp[i]=='(' || exp[i]=='{' || exp[i]=='[')
        	push(exp[i]);
        if(exp[i]==')' || exp[i]=='}' || exp[i]==']')
        	if(isempty())
        	{
			 	printf("Ill braketed\n");
              	exit(0);
        	}
            else
            {
            	temp=pop();
                if(!match(temp, exp[i]))
                {
                	printf("Ill braketed\n");
                	exit(0);
                }
            }
    }
    if(isempty())
      	printf("Well braketed\n");
    else       
    	printf("Ill braketed\n"); 
}
int main() 
{	
   	char exp[MAXSIZE];
   	printf("Enter an algebraic expression:");
    gets(exp);
    bracketmatch(exp);
}
