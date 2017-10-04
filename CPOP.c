/*******************************************************
	    CPOP IMPLEMENTATION
********************************************************/
//NOTES
//COMMUNICATION COST MATRIX(data[][]) HAS -1 VALUE FOR NON COMMUNICATING TASKS


#include<stdio.h>
#include<stdlib.h>
long no_tasks,no_machines; 
double **computation_costs,**data_transfer_rate,**data,*tasks_upper_rank,*tasks_lower_rank,*priority,*CriticalPath,LengthOfCriticalPath;//change 1
long *sorted_tasks;
struct TaskProcessor
{
       long processor;
       double AST;
       double AFT;
};

struct TaskProcessor *schedule;
/*******************************************************
	FUNCTION FOR DETERMINATION OF UPPER RANK	
********************************************************/
//CALCULATE AVG COMMUNCATION COST AND GIVE FEED TO SORTED TASK ARRAY
void insertlongo(long task,double rank)
{
     static long pos;
     long i;
     for(i=pos-1;i>=0;i--)
        if(rank>tasks_upper_rank[sorted_tasks[i]]) 
            sorted_tasks[i+1]=sorted_tasks[i];
        else
            break;
     sorted_tasks[i+1]=task;
     pos++;
}
double avg_communicationcost(long source,long destination)
{
       long i,j;
       double avg=0.0;
       for(i=0;i<no_machines;i++)
        {  for(j=0;j<no_machines;j++)
          {
             if(data_transfer_rate[i][j]!=0)
             	avg+=(data[source][destination]/data_transfer_rate[i][j]);
          }}
        double nmac=(double)(no_machines*no_machines-no_machines);
       avg=avg/nmac;
       return avg;
}
//change 3
double calculate_lower_rank(long task)
{
  long j,i;
  double avg_communication_cost,predecessor,avg=0.0,max=0.0,rank_predecessor;
    
    for(j=no_tasks-1;j>=0;j--)
    {
        if(data[j][task]!=-1)             //check if a successor
        {
           avg_communication_cost=avg_communicationcost(j,task);

           for(i=0;i<no_machines;i++)
              avg+=computation_costs[j][i];
            avg/=no_machines;


           if(tasks_lower_rank[j]==-1)
           {
              if(j==0)
                rank_predecessor=tasks_lower_rank[j]=0.0;
              else
                rank_predecessor= tasks_lower_rank[j]= calculate_lower_rank(j);
              //insertlongo(j,rank_successor);
           }
           else
               rank_predecessor= tasks_lower_rank[j];     
           predecessor=avg_communication_cost+rank_predecessor+avg;
           if(max<=predecessor)
              max=predecessor;
        }
     }
     return max;
}

double calculate_upper_rank(long task)
{
	long j;
	double avg_communication_cost,successor,avg=0.0,max=0,rank_successor;
    for(j=0;j<no_machines;j++)
         avg+=computation_costs[task][j];
    avg/=no_machines;
    for(j=0;j<no_tasks;j++)
    {
        if(data[task][j]!=-1)             //check if a successor
        {
           avg_communication_cost=avg_communicationcost(task,j);
           if(tasks_upper_rank[j]==-1)
           {
              rank_successor= tasks_upper_rank[j]= calculate_upper_rank(j);
              insertlongo(j,rank_successor);
           }
           else
               rank_successor= tasks_upper_rank[j];     
           successor=avg_communication_cost+rank_successor;
           if(max<successor)
              max=successor;
        }
     }
     return(avg+max);
}

/*******************************************************
	FUNCTION FOR DETERMINATION OF SCHEDULE	 
********************************************************/
void insertslots(double **machineFreeTime,long current_pos, double start,double end)
{
	long i;
	//prlongf("%lf %lf\n",start,end);
	if(start < 0)
		start=0;
	for(i=current_pos-1;i>=0;i--)
	{
		if(start < machineFreeTime[i][0])
		{
			machineFreeTime[i+1][0]=machineFreeTime[i][0];
			machineFreeTime[i+1][1]=machineFreeTime[i][1];
		}
		else
			break;
	}
	machineFreeTime[i+1][0]=start;
	machineFreeTime[i+1][1]=end;
}
void findfreeslots(long processor,double **machineFreeTime,long *noslots)
{
       long i,j;
       *noslots=0;
       double highest_AFT=-99999.0,min=99999.0;
       for(i=0;i<no_tasks;i++)
       {
           min=99999.0;
           if(schedule[i].processor==processor)
           {
	        if(schedule[i].AFT>highest_AFT)
           		highest_AFT=schedule[i].AFT;
           	for(j=0;j<no_tasks;j++)
           	{
               		if((i==j) || (schedule[j].processor!=processor))
                       		continue;
               		if((schedule[j].AST>=schedule[i].AFT) && (schedule[j].AST<min))
               		{
                       		min=schedule[j].AST;
               		}
           	}
           	if(min<99998.0)
           	{
               		insertslots(machineFreeTime,*noslots,schedule[i].AFT,min);
               		(*noslots)++;
           	}
           }
       }
       insertslots(machineFreeTime,*noslots,highest_AFT,99999.0);
       (*noslots)++;
}
long isEntryTask(long task)
{
	long i;
	for(i=0;i<no_tasks;i++)
	{
		if(data[i][task]!=-1)
			return 0;
	}
	return 1;
}
double find_EST(long task,long processor)
{
	long i;
	double ST,EST=-99999.0,comm_cost;
	for(i=0;i<no_tasks;i++)
	{
		if(data[i][task]!=-1)
		{
			if(data_transfer_rate[schedule[i].processor][processor]==0)
				comm_cost=0;
			else
				comm_cost=data[i][task]/data_transfer_rate[schedule[i].processor][processor];
			ST=schedule[i].AFT + comm_cost;
			if(EST<ST)
				EST=ST;
		}
	}
	return EST;
}
void calculate_EST_EFT(long task,long processor,struct TaskProcessor *EST_EFT)
{
	double **machineFreeTime,EST;
	long i;
	machineFreeTime=(double**)calloc(500,sizeof(double*));
     	for(i=0;i<100;i++)
        	 machineFreeTime[i]=(double*)calloc(2,sizeof(double));
        long noslots=0;
        findfreeslots(processor,machineFreeTime,&noslots);
        //if(task==2)
        //for(i=0;i<noslots;i++)
        //{
        //	prlongf("%lf %lf\n",machineFreeTime[i][0],machineFreeTime[i][1]);
        //}
        //prlongf("\n\n"); 
        EST=find_EST(task,processor);
        //prlongf("%lf\n",EST);
        EST_EFT->AST=EST;
        EST_EFT->processor=processor;
        for(i=0;i<noslots;i++)
        {
        	if(EST<machineFreeTime[i][0])
        	{
        		if((machineFreeTime[i][0] + computation_costs[task][processor]) <= machineFreeTime[i][1])
        		{
        			EST_EFT->AST=machineFreeTime[i][0];
        			EST_EFT->AFT=machineFreeTime[i][0] + computation_costs[task][processor];
        			return;
        		}
        	}
        	if(EST>=machineFreeTime[i][0])
        	{
        		if((EST + computation_costs[task][processor]) <= machineFreeTime[i][1])
        		{
        			EST_EFT->AFT=EST_EFT->AST + computation_costs[task][processor];
        			return;
        		}
        	}
        }
}
void make_schedule()
{
     long i,j,k,t=0,processor,task;
     double minCost=9999.99,min_EFT=9999.99;
     struct TaskProcessor *EST_EFT;
     EST_EFT=(struct TaskProcessor *)calloc(1,sizeof(struct TaskProcessor));
     for(i=0;i<no_tasks;i++)
     {
        min_EFT=9999.99;
        task=sorted_tasks[i];
        if(isEntryTask(task))
        {
            for(j=0;j<no_machines;j++)
            {
                if(minCost>computation_costs[task][j])
                {
                     minCost=computation_costs[task][j];
                     processor=j;
                }
            }
            schedule[task].processor=processor;
            schedule[task].AST=0;
            schedule[task].AFT=minCost;
        }
        else
        {
            for(j=0;j<no_machines;j++)
            {
            	calculate_EST_EFT(task,j,EST_EFT);
            	printf("%lf %lf %ld\n",EST_EFT->AST,EST_EFT->AFT,EST_EFT->processor);
            	if(min_EFT>(EST_EFT->AFT))
            	{
            		schedule[task]=*EST_EFT;
            		min_EFT=EST_EFT->AFT;
            	}     
            }
        }
      
        printf("Task scheduled %ld\n",task);
        printf("%ld %lf %lf\n",schedule[task].processor,schedule[task].AST,schedule[task].AFT);
     }
}

/*******************************************************
	FUNCTION FOR DISPLAYING SCHEDULE	 
********************************************************/
void display_schedule()
{
	long i;
	printf("\n\nSCHEDULE\n\n");
	for(i=0;i<no_tasks;i++)
	{
		printf("TASK :%ld PROCESSOR :%ld AST :%lf AFT :%lf\n",i+1,schedule[i].processor+1,schedule[i].AST,schedule[i].AFT);
	}
}


long main()
{
	
	long i,j;
	FILE *fp;
	fp=fopen("Input.txt","r+");
	//prlongf("Enter the number of tasks and machines\n");
	fscanf(fp,"%ld%ld",&no_tasks,&no_machines);
	//Initialize Arrays
	computation_costs=(double**)calloc(no_tasks,sizeof(double*));
	for(i=0;i<no_tasks;i++)
        	computation_costs[i]=(double*)calloc(no_machines,sizeof(double));
	data_transfer_rate=(double**)calloc(no_machines,sizeof(double*));
	for(i=0;i<no_machines;i++)
    	data_transfer_rate[i]=(double*)calloc(no_machines,sizeof(double));
	data=(double**)calloc(no_tasks,sizeof(double*));
	for(i=0;i<no_tasks;i++)
    	data[i]=(double*)calloc(no_tasks,sizeof(double));
	tasks_upper_rank=(double *)calloc(no_tasks,sizeof(double));
  tasks_lower_rank=(double *)calloc(no_tasks,sizeof(double));//change2
	for(i=0;i<no_tasks;i++)
       {
        tasks_upper_rank[i]=-1;
        tasks_lower_rank[i]=-1;
       }
    	sorted_tasks=(long *)calloc(no_tasks,sizeof(long));
    	schedule=(struct TaskProcessor*)calloc(no_tasks,sizeof(struct TaskProcessor));
    	for(i=0;i<no_tasks;i++)
    		schedule[i].processor=-1;
	//prlongf("Enter the computation costs of each task\n");
	for(i=0;i<no_tasks;i++)
		for(j=0;j<no_machines;j++)
			fscanf(fp,"%lf",&computation_costs[i][j]);
	//prlongf("Enter the matrix of data transfer rate between two processors\n");
	for(i=0;i<no_machines;i++)
		for(j=0;j<no_machines;j++)
			fscanf(fp,"%lf",&data_transfer_rate[i][j]);
	//prlongf("Enter the matrix of data to be transferred between two tasks\n");
	for(i=0;i<no_tasks;i++)
	for(j=0;j<no_tasks;j++)
		fscanf(fp,"%lf",&data[i][j]);



  //Allocating the memory to the Priority(Upper Rank + Lower Rank):

  priority=(double*)calloc(no_tasks,sizeof(double));

  // Allocating the memory to CriticalPath(One  which takes the highest time in the graph)
  CriticalPath=(double*)calloc(no_tasks,sizeof(double));
	//calculate upper rank
	for(i=0;i<no_tasks;i++)
	{
    		if(tasks_upper_rank[i]==-1)
    		{
            		tasks_upper_rank[i]=calculate_upper_rank(i);
            		insertlongo(i,tasks_upper_rank[i]);
        	}
	}

    //change 4
  for(i=no_tasks-1;i>=0;i--)
  {
    if(tasks_lower_rank[i]==-1)
    {
      tasks_lower_rank[i]=calculate_lower_rank(i);
    }

  }

    printf("\n\nLower RANKS OF TASKS :\n\n");
  for(i=0;i<no_tasks;i++)
          printf("TASK NO. %ld: %.3lf\n",i,tasks_lower_rank[i]);


    	printf("UPPER RANKS OF TASKS :\n\n");
	for(i=0;i<no_tasks;i++)
        	printf("TASK NO. %ld: %.3lf\n",i,tasks_upper_rank[i]);


  for( i=0;i<no_tasks;++i)
  {

    priority[i]=tasks_upper_rank[i]+tasks_lower_rank[i];
    printf("TASK NO. %ld: %.3lf\n",i,priority[i]);

  }


  //Critical_path
  // var(s) needed are 'LengthOfCriticalPath' and CriticalSet
    LengthOfCriticalPath=priority[0];

    	for(i=0;i<no_tasks;i++)
        	printf("TASK NO. : %ld\n",sorted_tasks[i]);
    	make_schedule();
    	display_schedule();
    	scanf("%ld",&i);
}
