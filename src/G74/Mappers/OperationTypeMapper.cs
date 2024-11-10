using G74.DTO;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Aggregates.OperationType;

public class OperationTypeMapper
{

    public static OperationTypeDTO DataModelToDTO(OperationTypeDataModel operationTypeDataModel)
    {
        return new OperationTypeDTO(
            operationTypeDataModel.operationTypeID, 
            new Name(operationTypeDataModel.name),
            new RequiredStaffBySpecialization(makeStringIntoDictionary(operationTypeDataModel.requiredStaffBySpecialization)), 
            new Duration(operationTypeDataModel.estimatedDuration)
        );
    }

    public static OperationType DTOtoDomain(OperationTypeDTO operationTypeDTO){
        return new OperationType(
            operationTypeDTO.operationTypeID,
            operationTypeDTO.name,
            operationTypeDTO.requiredStaffBySpecialization,
            operationTypeDTO.duration.DurationTime
        );
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
    private static Dictionary<StaffSpecialization,int> makeStringIntoDictionary(string specializationStaffList){
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