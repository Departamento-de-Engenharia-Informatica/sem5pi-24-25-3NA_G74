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

    private static Dictionary<StaffSpecialization, int> makeStringIntoDictionary(string specializationStaffList)
    {
        // Divide a string principal em itens separados por ";"
        List<string> list = specializationStaffList.Split(';').ToList();
        List<string> specializations = new List<string>();
        List<int> quantities = new List<int>();

        foreach (string item in list)
        {
            // Para cada item, divide em 2 partes separadas por vírgula
            string[] split = item.Split(',');

            // Verifica se o item está no formato esperado
            if (split.Length != 2)
            {
                Console.WriteLine($"Invalid item format in specializationStaffList: {item}");
                continue; // Ignora item inválido
            }

            // A primeira parte é a quantidade, a segunda parte é a especialização
            if (int.TryParse(split[0], out int quantity))  // A quantidade
            {
                specializations.Add(split[1]); // A especialização
                quantities.Add(quantity);
            }
            else
            {
                Console.WriteLine($"Invalid quantity in specializationStaffList: {split[0]}");
            }
        }

        // Agora criamos o dicionário a partir das listas de especializações e quantidades
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