using G74.Domain.Shared;
using G74.Domain.Value_Objects.SharedValueObjects;

public class DataOperationType : Entity<Guid>
{
    

    public Guid Id { get; protected set; } 
    public Name Name {get; set; }
    public RequiredStaffBySpecialization RequiredStaffBySpecialization {get;private set; }
    public Duration EstimatedDuration {get;private set; }

    public DataOperationType( Name name, RequiredStaffBySpecialization requiredStaffBySpecialization, Duration estimatedDuration) : base(Guid.NewGuid()){
        Name = name;
        RequiredStaffBySpecialization = requiredStaffBySpecialization;
        EstimatedDuration = estimatedDuration;
    }
}