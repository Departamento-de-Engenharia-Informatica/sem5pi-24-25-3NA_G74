using Azure;
using G74.Domain.Value_Objects.SharedValueObjects;

public class OperationTypeDTO
{

    public string? operationTypeID { get; set; }
	public string? name { get; set; }
	public string? requiredStaffBySpecialization { get; set; }
	public string? duration { get; set; }

	public OperationTypeDTO()
	{
		
	}
    
    public OperationTypeDTO(string? operationTypeID, string? name, string? requiredStaffBySpecialization, string? duration)
    {
        this.operationTypeID = operationTypeID;
		this.name = name;
		this.requiredStaffBySpecialization = requiredStaffBySpecialization;
		this.duration = duration;
    }
}