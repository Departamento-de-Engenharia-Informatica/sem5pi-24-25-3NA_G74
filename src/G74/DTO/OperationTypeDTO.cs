using Azure;
using G74.Domain.Value_Objects.SharedValueObjects;

public class OperationTypeDTO
{

    public string operationTypeID { get; }
	public string name { get; }
	public string requiredStaffBySpecialization { get; }
	public string duration { get; }

    
    public OperationTypeDTO(string operationTypeID, string name, string requiredStaffBySpecialization, string duration)
    {
        this.operationTypeID = operationTypeID;
		this.name = name;
		this.requiredStaffBySpecialization = requiredStaffBySpecialization;
		this.duration = duration;
    }
}