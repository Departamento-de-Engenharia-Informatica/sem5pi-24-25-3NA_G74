using G74.Domain.Shared;
using G74.Domain.Value_Objects.SharedValueObjects;

public class OperationType : IValueObject
{
    public Name Name {get; set; }
    public RequiredStaffBySpecialization RequiredStaffBySpecialization {get;private set; }
    public Duration EstimatedDuration {get;private set; }

    public OperationType(Name name, RequiredStaffBySpecialization requiredStaffBySpecialization, Duration estimatedDuration) {
        Name = name;
        RequiredStaffBySpecialization = requiredStaffBySpecialization;
        EstimatedDuration = estimatedDuration;
    }
}