

using G74.Domain.Aggregates.OperationType;
using G74.Domain.Shared;

namespace G74.DTO;

public class OperationTypeDataModel : Entity<Guid>
{
    public int OperationTypeID { get; private set; }
    public string Name { get; private set; }
    public string RequiredStaffBySpecialization { get; private set; }
    public int EstimatedDuration { get; private set; }

    protected OperationTypeDataModel() : base(Guid.NewGuid())
    {
        
    }
	
    public OperationTypeDataModel(int operationTypeId, string name, string requiredStaffBySpecialization, int estimatedDuration) : base(Guid.NewGuid())
    {
        OperationTypeID = operationTypeId;
        Name = name;
        RequiredStaffBySpecialization = requiredStaffBySpecialization;
        EstimatedDuration = estimatedDuration;
    }
    
    public void UpdateName(string newName)
    {
        Name = newName;
    }

    public void UpdateRequiredStaffBySpecialization(string newRequiredStaffBySpecialization)
    {
        RequiredStaffBySpecialization = newRequiredStaffBySpecialization;
    }

    public void UpdateEstimatedDuration(string newEstimatedDuration)
    {
        EstimatedDuration = int.Parse(newEstimatedDuration);
    }
}