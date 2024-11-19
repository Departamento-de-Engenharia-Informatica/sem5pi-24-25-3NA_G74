using G74.Domain.Aggregates.OperationType;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;

namespace G74.Mappers;

public class OperationTypeDtoMapper
{
    public OperationTypeDTO OperationTypeToDto(OperationType operationType)
    {
        if (operationType == null)
        {
            throw new ArgumentNullException(nameof(operationType), "Operation type cannot be null.");
        }
        
        OperationTypeDTO operationTypeDto = new OperationTypeDTO(operationType.operationTypeID.ToString(),operationType.name.ToString(),operationType.requiredStaffBySpecialization.SpecializationStaffList.ToString(),operationType.duration.ToString());

        return operationTypeDto;
    }

    public OperationType OperationTypeDtoToDomain(OperationTypeDTO operationTypeDto)
    {
        if (operationTypeDto == null)
        {
            throw new ArgumentNullException(nameof(operationTypeDto), "Operation type cannot be null.");
        }

        OperationType operationType = new OperationType(int.Parse(operationTypeDto.operationTypeID),
            new Name(operationTypeDto.name),
            new RequiredStaffBySpecialization(makeStringIntoDictionary(operationTypeDto.requiredStaffBySpecialization)),
            (int.Parse(operationTypeDto.duration)));
        return operationType;
    }
    
    
    
    private static Dictionary<StaffSpecialization,int> turnListIntoDictionary(List<string> specializationStaffList, List<int> quantities)
    {
        Dictionary<StaffSpecialization,int> dictionary = new Dictionary<StaffSpecialization,int>();
        for (int i = 0; i < specializationStaffList.Count; i++)
        {
            dictionary.Add(new StaffSpecialization(specializationStaffList[i]), quantities[i]);
        }
        return dictionary;
    } 

    //example input: "Surgeon:1,Anesthesiologist:2"
    public Dictionary<StaffSpecialization,int> makeStringIntoDictionary(string specializationStaffList){
        List<string> list = specializationStaffList.Split(',').ToList();
        List<string> specializations = new List<string>();
        List<int> quantities = new List<int>();
        foreach (string item in list)
        {
            string[] split = item.Split(':');
            specializations.Add(split[0]);
            quantities.Add(int.Parse(split[1]));
        }
        return turnListIntoDictionary(specializations, quantities); 
    }

}