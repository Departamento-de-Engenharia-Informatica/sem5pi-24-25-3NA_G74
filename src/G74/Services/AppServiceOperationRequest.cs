
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Services;

public class AppServiceOperationRequest : IAppServiceOperationRequest 
{
    private readonly IOperationRequestRepository _operationRepository;
    private readonly IPatientAppService _patientAppService;
    private readonly IPatientRepository _patientRepository;
    

    public AppServiceOperationRequest(IOperationRequestRepository operationRepository)
    {
        _operationRepository = operationRepository;
        
    }

    public Task<OperationRequestDTO> GetOperationRequestById(long id)
    {
        throw new NotImplementedException();
    }

    public async Task<OperationRequestDTO> RegisterOperationRequest(CreateOperationRequestDTO operationDto)
    {
        if(!Enum.TryParse<Priority.PriorityType>(operationDto.Priority.ToString(),true, out var priority))
        {
            throw new ArgumentException("Invalid priority");
        }

        if(_patientAppService==null){
            Console.WriteLine("Pois");
        }
        
        if(_patientRepository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(operationDto.MedicalRecordNumber)) == null)
        {
            throw new ArgumentException("No patient found with this medical record number");
        }

        var operation = await new OperationRequestBuilder(
            new MedicalRecordNumber(operationDto.MedicalRecordNumber),
            new LicenceNumber(operationDto.LicenceNumber),
            new OperationType(new Name(operationDto.Name), new RequiredStaffBySpecialization(operationDto.RequiredStaffBySpecialization),operationDto.EstimatedDuration),
            new DeadlineDate(operationDto.DeadlineDate),
            new Priority(operationDto.Priority)
        ).Build();
        

        var operationDataModel = OperationRequestMapper.ToDataModel(operation); 
        await _operationRepository.Add(operationDataModel);
        return OperationRequestMapper.ToDTO(operation);
    }

    public Task<OperationRequestDTO> UpdateOperationRequest(long id, OperationRequestDTO updatedOperationDto)
    {
        throw new NotImplementedException();
    }
}