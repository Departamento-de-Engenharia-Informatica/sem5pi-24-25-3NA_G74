using Azure;
using G74.Domain.Value_Objects.SharedValueObjects;

public class OperationTypeDTO
{

    public long operationTypeID { get; }
	public Name name { get; }
	public RequiredStaffBySpecialization requiredStaffBySpecialization { get; }
	public Duration duration { get; }

    
    public OperationTypeDTO(long operationTypeID, Name name, RequiredStaffBySpecialization requiredStaffBySpecialization, Duration duration)
    {
        this.operationTypeID = operationTypeID;
		this.name = name;
		this.requiredStaffBySpecialization = requiredStaffBySpecialization;
		this.duration = duration;
    }
}