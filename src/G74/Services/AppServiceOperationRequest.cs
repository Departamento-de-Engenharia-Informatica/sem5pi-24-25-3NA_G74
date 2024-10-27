
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Services;

public class AppServiceOperationRequest : IAppServiceOperationRequest 
{
    private readonly IOperationRequestRepository _operationRepository;
    private readonly IPatientAppService _patientAppService;
    

    public AppServiceOperationRequest(IOperationRequestRepository operationRepository)
    {
        _operationRepository = operationRepository;
        
    }

    public async Task<OperationRequestDTO> DeleteOperationRequest(Guid id)
    {
        var operation = await _operationRepository.Delete(id);
        if(operation!=null){return OperationRequestMapper.ToDTO(operation);}else{throw new Exception("Operation Executed Not as Expected.");};
    }

    public Task<OperationRequestDTO> GetOperationRequestById(long id)
    {
        throw new NotImplementedException();
    }

    public async Task<List<OperationRequestDTO>> Read()
    {
       var operationRequests = await _operationRepository.ReadAll();
            var operationRequestsDTO = new List<OperationRequestDTO>();

            foreach (var operationRequest in operationRequests)
            {
                var operationRequestDTO = OperationRequestMapper.ToDTO(operationRequest);
                operationRequestsDTO.Add(operationRequestDTO);
            }

            return operationRequestsDTO;
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
        
        // if(_patientAppService.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(operationDto.MedicalRecordNumber))==null)
        // { 
        //     throw new ArgumentException("No patient found with this medical record number");
        // }

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

    public async Task<OperationRequestDTO> UpdateOperationRequest(Guid id, OperationRequestDTO updatedOperationDto)
    {
        
        if (updatedOperationDto == null)
    {
        throw new ArgumentNullException(nameof(updatedOperationDto), "Updated operation request data transfer object cannot be null");
    }
    
    var existingOperation = await _operationRepository.GetOperationRequestByIdAsync(id);
    if (existingOperation == null)
    {
        throw new ArgumentException("No operation found with this ID");
    }
    Console.WriteLine("Operation");
  
    existingOperation.MedicalRecordNumber = new MedicalRecordNumber(updatedOperationDto.MedicalRecordNumber.MedicalNumber);
    existingOperation.LicenceNumber = new LicenceNumber(updatedOperationDto.LicenceNumber.licenceNumber);
    existingOperation.OperationType = new OperationType(
        new Name(updatedOperationDto.OperationType.Name.ToString()),
        new RequiredStaffBySpecialization(updatedOperationDto.OperationType.RequiredStaffBySpecialization.SpecializationStaffList),
        updatedOperationDto.OperationType.EstimatedDuration
    );
    existingOperation.DeadlineDate = new DeadlineDate(updatedOperationDto.DeadlineDate.date);
    existingOperation.Priority = new Priority(updatedOperationDto.Priority.PriorityDescription);

    
    await _operationRepository.Update(id,existingOperation);

    
    return OperationRequestMapper.ToDTO(existingOperation);
    }

   
}