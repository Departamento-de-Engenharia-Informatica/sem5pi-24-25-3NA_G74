using G74.Domain.Shared;



public class Priority : IValueObject
{
   public enum PriorityType{
        ElectiveSurgery,
        UrgentSurgery,
        EmergencySurgery
   } 
   public PriorityType PriorityDescription { get; private set;}

   
}

