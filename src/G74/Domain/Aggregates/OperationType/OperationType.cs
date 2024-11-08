
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain.Aggregates.OperationType;

public class OperationType
{
	public int operationTypeID { get; }
	public Name name { get; }
	public RequiredStaffBySpecialization requiredStaffBySpecialization { get; }
	public int duration { get; }

	public OperationType(int operationTypeID, Name name, RequiredStaffBySpecialization requiredStaffBySpecialization, int duration)
	{
		this.operationTypeID = operationTypeID;
		this.name = name;
		this.requiredStaffBySpecialization = requiredStaffBySpecialization;
		this.duration = duration;
	}
    
}