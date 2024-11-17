using G74.DTO;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Aggregates.OperationType;

public class OperationTypeToDataModelMapper
{

    public OperationTypeDataModel OperationTypeDomainToDataModel(OperationType operationType)
    {
        return new OperationTypeDataModel(
            operationType.operationTypeID,
            operationType.name.ToString(),
            operationType.requiredStaffBySpecialization.ToString(),
            operationType.duration
        );
    }

    public OperationType OperationTypeDataModelToDomain(OperationTypeDataModel operationTypeDataModel){
        return new OperationType(
            operationTypeDataModel.OperationTypeID,
            new Name(operationTypeDataModel.Name),
            new RequiredStaffBySpecialization(makeStringIntoDictionary(operationTypeDataModel.RequiredStaffBySpecialization)),
            operationTypeDataModel.EstimatedDuration
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
    
    public bool UpdateDataModel(OperationTypeDataModel operationTypeDataModel, OperationType operationType)
    {
        operationTypeDataModel.UpdateName(operationType.name.ToString());
        operationTypeDataModel.UpdateRequiredStaffBySpecialization(operationType.requiredStaffBySpecialization.ToString());
        operationTypeDataModel.UpdateEstimatedDuration(operationType.duration.ToString());
        return true;
    }

    public IEnumerable<OperationType> ToDomain(IEnumerable<OperationTypeDataModel> operationTypeDataModels)
    {
        var operationTypeList = operationTypeDataModels.Select(OperationTypeDataModelToDomain).ToList();

        return operationTypeList.AsEnumerable();
    }
}