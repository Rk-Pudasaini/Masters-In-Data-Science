#include <stdio.h>
#include<ctype.h>
#define MAXSIZE 100       
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
int main()
{
	char postfix[MAXSIZE];
	int i = 0, a, b, val;
	printf("Ente postfix expression:");
	gets(postfix);
	while(postfix[i] != '\0')
	{
		if(isdigit(postfix[i]))
			push(postfix[i]-48);
		else
		{
			a = pop();
			b = pop();
			switch (postfix[i])
        	{
            	case '*':
                	val = b * a;
                	break;
            	case '/':
                	val = b / a;
                	break;
            	case '+':
                	val = b + a;
                	break;
            	case '-':
                	val = b - a;
                	break;
        	}
        	push(val);
		}				
		i++;	
	}
	printf("Result of postfix evaluation: %d", pop());
}

