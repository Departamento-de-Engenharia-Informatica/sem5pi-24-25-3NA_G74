

using G74.Domain.Aggregates.OperationType;
using G74.Domain.Shared;

namespace G74.DTO;

public class OperationTypeDataModel : Entity<Guid>
{
    public int operationTypeID { get; private set; }
    public string name { get; private set; }
    public string requiredStaffBySpecialization { get; private set; }
    public int estimatedDuration { get; private set; }

    protected OperationTypeDataModel() : base(Guid.NewGuid())
    {
        
    }
	
    public OperationTypeDataModel(OperationType operationType) : base(Guid.NewGuid()){
        operationTypeID = operationType.operationTypeID;
        name = operationType.name.ToString();
        requiredStaffBySpecialization = operationType.requiredStaffBySpecialization.ToString();
        estimatedDuration = operationType.duration;
    }
}