using Azure;
using G74.Domain.Value_Objects.SharedValueObjects;

public class OperationTypeDTO
{

    public string? operationTypeId { get; set; }
	public string? name { get; set; }
	public string? requiredStaffBySpecialization { get; set; }
	public string? duration { get; set; }

	public OperationTypeDTO()
	{
		
	}
    
    public OperationTypeDTO(string? operationTypeId, string? name, string? requiredStaffBySpecialization, string? duration)
    {
        this.operationTypeId = operationTypeId;
		this.name = name;
		this.requiredStaffBySpecialization = requiredStaffBySpecialization;
		this.duration = duration;
    }
}