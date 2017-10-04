
# include <stdio.h>
# include <stdlib.h>
# include <iostream>
# include <fstream>
# include <math.h>
# include <vector>
//# define nop 5    /*nop is the number of processors*/

using namespace std;

int pro[4]              =   {2,3,4,5};
float shapeparameter[3] =   {0.5,1,2};
float ccr[5]            =   {0.1,0.5,1,5,10};
int *wid,*h,v=0,nop;
FILE *fp;
fstream output_file;



int task();                                  /*Declaration of all the functions*/
float ratio();
float graphshape();
int avgcomp();
int width(float,int);
int out_degree(int);
int below(int);
float compcost(int);
int commcost(int ,float);
void end();
void output(int*,float*);
void allocatePro();

int main()
    {
    srand(time(NULL));
    int n,k,wDAG,height,nog,*outdegree,i,r,j,temp,*cost;
    float alpha,ccratio,*comp;
    
    fp=fopen("RandomTaskGRaphs.txt","w");         /*Output File*/
    output_file.open("Input.txt",ios::out); //Input File line-2
    
    allocatePro();
    printf("How many graphs you want to generate?\n");
    scanf("%d",&nog);    
    
    for(k=0;k<nog;k++)
          {
          v=0;
          n=task();
            printf("%d",n);
            
          ccratio=ratio();
          alpha=graphshape();
          wDAG=avgcomp();
          
          height=(int)(sqrt(n)/alpha);
          //height=5;
          fprintf(fp,"\n\nGraph # %d \nn=%d, alpha=%f,CC Ratio=%f ,Average Computational Cost=%d,\theight=%d \n\n\n",k+1,n,alpha,ccratio,wDAG,height);
                    
          h=(int*)calloc(height,sizeof(int));
          for(i=0;i<height;i++)                          /*Storing no of edges in each level into an array*/
                  {
                  h[i]=width(alpha,n);
                  fprintf(fp,"width of level %d=%d\n",i+1,h[i]);
                  if(i==0)
                    h[i]=1;
                  v=v+h[i];
                  }
                  cout<<v<<endl;
          
          fprintf(fp,"\nFinal number of vertices=%d\n\n",v);
           output_file<<v<<" "<<nop<<endl; //Line-3 input file

          cost      =(int*)calloc(v*v,sizeof(int));     /*Communication Cost matrix*/
          outdegree =(int*)calloc(v,sizeof(int));           /*Array to store outdegree of each vertex*/
          comp      =(float*)calloc(v*nop,sizeof(float));   /*Computational Cost matrix*/
    
          for(i=0;i<v*v;i++)
               cost[i]=-1;
    
          for(i=0;i<v;i++)                                  /*Storing outdegree of each vertex in an array*/
          {
               outdegree[i]=out_degree(i);  
               //if(i!=v-1 && outdegree[i]==0)
                  //outdegree[i]=1;      
               fprintf(fp,"outdegree of %d=%d\n",i+1,outdegree[i]);
          }
          
         for(i=0;i<v;i++)              
               {
               for(j=0;j<=outdegree[i];j++)                        
                 {
                 if(below(i+1))
                               temp=(rand()%below(i+1)+(v-below(i+1)));/*Randomly selecting vertices to be connected with given vertex*/
                               cost[i+temp*v]=commcost(wDAG,ccratio);     /*Storing the value of Communication cost between Vertices*/
                 }          
               }

               vector<int> ccol,crow;
               //Correcting matrix for valid input
               for( i=1;i<v;++i)
               {
                  int flag=0;
                  for(int j=1;j<=i;++j)
                  { 
                      if(cost[i+j*v]!=-1)
                           flag=1;
                  }
                  if(flag==0)
                    ccol.push_back(i);
               }
               

               for( i=0;i<ccol.size();++i)
               
                cost[ccol[i]-1+(v*(ccol[i]))]=commcost(wDAG,ccratio);
               

              for( i=0;i<v-1;++i)
               {
                  int flag=0;
                  for(int j=i+1;j<v;++j)
                  {
                      if(cost[j+i*v]!=-1)
                    
                        flag=1;  
                  }
                  if(flag==0)
                    crow.push_back(i);
               }
              
                for( i=0;i<crow.size();++i)
                
                cost[crow[i]+(v*(crow[i]+1))]=commcost(wDAG,ccratio);

              for(i=0;i<v;++i)
              {
                if(cost[i+v*(v-1)]!=-1)
                  cost[i+v*(v-1)]=-1;
              }
               

          
          for(i=0;i<nop;i++)
               for(j=0;j<v;j++)
                       comp[i+j*nop]=compcost(wDAG);        /*Storing the value of Computation cost of a process for each task*/
          output(cost,comp);
          
          }
    fclose(fp);
    end();
    scanf("%d",&i);
    }

void allocatePro()
{
 nop=pro[rand()%4];
 // nop=pro[0];
}
    
int task()                    /*Function to randomly generate number of tasks*/
    {
    int r,x;
    r=rand()%10;
  
    x=(r+1)*10;
    return(x);         
    }
    
float ratio()                 /*Function to randomly determine Communication to Computation Ratio*/
      {
      int temp;
      temp=rand()%5;
      return(ccr[temp]);     
      }

float graphshape()           /*Function to randomly determine Shape Parameter*/
    {
    int temp;
    temp=rand()%3;
    return(shapeparameter[temp]);
    }

int avgcomp()                    /*Function to randomly determine average computational time*/                    
    {
    int r,cos;
    r=rand()%4;
    cos=(r+1)*10;
    return(cos);         
    }

int width(float alpha,int x)     /*Function to randomly determine number of vertices at each level*/
    {
    int i,mean,temp;
    mean=(int)(alpha*sqrt(x));
    wid=(int*)calloc(2*mean-1,sizeof(int));
    for(i=0;i<(2*mean-1);i++)
          wid[i]=i+1;
    temp=rand()%(2*mean-1);
    return(wid[temp]);
    }    

int out_degree(int i)            /*Function to randomly determine number of outgoing edges*/
    {
    int r,temp;
    temp=below(i+1);
    if(!temp)return 0;
    else    return((rand()%temp)+1);     
    }  

int below(int i)                  /*Function to Calculate number of vertices at level lower than any of the given vetex*/
    {
    int j=0,temp=0;
    while(temp<i)
         temp=temp+h[j++]; 
    return(v-temp);
    }

float compcost(int wDAG)          /*Function to randomly determine Computation cost of any Task*/
    {
    int r;
    float cos;
    cos=(rand()%(2*wDAG)+1);
    return(cos);
    }

int commcost(int wDAG,float ccratio) /*Function to randomly determine Communication cost between any two given Tasks*/
    {
    int cos,cDAG;
    
    cDAG=int(wDAG*ccratio);
    cos=(rand()%cDAG+1);
    return(cos);
    }
    
void output(int *cost,float *comp)   /*Function to Print output in a Text File*/
     {
     int i,j;

      fprintf(fp,"\nComputation matrix is\n\n");
     fprintf(fp,"---------\t");

     
     for(i=0;i<nop;i++)
           fprintf(fp,"*Pr%d*\t",(i+1));
    
     fprintf(fp,"\n");      
     
     for(j=0;j<v;j++)
          {
            fprintf(fp,"Task # %d\t",(j+1));
            
            for(i=0;i<nop;i++)
             {
               fprintf(fp,"%d\t",(int)(comp[j+i*v]+1));
                output_file<<(int)(comp[j+i*v]+1)<<"\t";
              }
            fprintf(fp,"\n"); 
            output_file<<endl;         
          }        
        //avg communication between the processors
          for(int i=0;i<nop;++i)
          {
            for(int j=0;j<nop;++j)
            {
              if(i!=j)
                output_file<<1<<"\t";
              else
                output_file<<0<<"\t";
            }
            output_file<<endl;
          }

          //Correcting cost for HEFT
         /* for(int i=0;i<v;++i)
          {
            for(int j=0;j<v;++j)
            {

            }
          }*/


     fprintf(fp,"\nCommunictaion matrix is\n\n");
     fprintf(fp,"--From / To---\t");
     for(i=0;i<v;i++)
                fprintf(fp,"#%d\t",i+1);              
    
     fprintf(fp,"\n");  
     for(i=0;i<v;i++)
           {
           fprintf(fp,"From Task # %d\t",(i+1));
           
           for(j=0;j<v;j++)
           {
                fprintf(fp,"%d\t",cost[i+j*v]);  
                output_file<<cost[i+j*v]<<"\t";
           }
           fprintf(fp,"\n");
           output_file<<endl;
           }
     
    
     }
 
 
   
void end()
     {
     system("cls");
     printf("\nYour Graphs have been saved in text file named RandomGraphs.txt");
     printf("\n\n\n\n\nPRESS ANY KEY to exit\n");
} 
