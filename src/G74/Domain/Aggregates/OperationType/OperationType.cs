
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain.Aggregates.OperationType;

public class OperationType
{
	public int operationTypeID { get; }
	public Name name { get; set; }
	public RequiredStaffBySpecialization requiredStaffBySpecialization { get; set; }
	public int duration { get; set; }

	public OperationType(int operationTypeID, Name name, RequiredStaffBySpecialization requiredStaffBySpecialization, int duration)
	{
		this.operationTypeID = operationTypeID;
		this.name = name;
		this.requiredStaffBySpecialization = requiredStaffBySpecialization;
		this.duration = duration;
	}
	
	public void UpdateName(Name newName)
	{
		name = newName ?? throw new ArgumentNullException(nameof(newName), "Name cannot be null when updating");
	}

	public void UpdateRequiredStaffBySpecialization(RequiredStaffBySpecialization newRequiredStaffBySpecialization)
	{
		requiredStaffBySpecialization = newRequiredStaffBySpecialization ??
		              throw new ArgumentNullException(nameof(newRequiredStaffBySpecialization), "Name cannot be null when updating");
	}

	public void UpdateDuration(int newDuration)
	{
		duration = newDuration;
	}
}