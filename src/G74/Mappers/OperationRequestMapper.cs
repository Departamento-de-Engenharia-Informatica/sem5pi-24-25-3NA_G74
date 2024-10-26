using System.Linq.Expressions;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

public class OperationRequestMapper 
{
    /**
    public static PatientDTO ToDTO(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient);

        return new PatientDTO(patient.Name, patient.Gender.GenderDescription,
            patient.DateOfBirth, patient.ContactInformation, patient.EmergencyContact);
    }

    public static Patient toDomain(PatientDTO patientDto)
    {
        throw new NotImplementedException();
    }

    public static PatientDataModel ToDataModel(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient, nameof(Patient));

        return new PatientDataModel(patient);

    }

    public static Patient FromDataModelToDomain(PatientDataModel patientDataModel)
    {
        ArgumentNullException.ThrowIfNull(patientDataModel, nameof(patientDataModel));

        return new Patient(patientDataModel);
    }
    */

    public static OperationRequestDTO ToDTO(OperationRequest operationRequest)
    {
        
        return new OperationRequestDTO(
            operationRequest.MedicalRecordNumber,
            operationRequest.LicenceNumber,
            operationRequest.OperationType,
            operationRequest.DeadlineDate,
            operationRequest.Priority);
    }

    public static OperationRequest ToDomain(OperationRequestDTO operationRequestDto)
    {
        return new OperationRequest(
            operationRequestDto.MedicalRecordNumber,
            operationRequestDto.LicenceNumber,
            operationRequestDto.OperationType,
            operationRequestDto.DeadlineDate,
            operationRequestDto.Priority
        );

    }
    public static DataOperationRequest ToDataModel(OperationRequest operationRequest)
    {
        return new DataOperationRequest(operationRequest);
    }
    public static OperationRequest FromDataModelToDomain(DataOperationRequest operationRequestDataModel)
    {  
        return new OperationRequest(
            new MedicalRecordNumber(operationRequestDataModel.MedicalRecordNumber),
            new LicenceNumber(operationRequestDataModel.LicenceNumber),
            new OperationType(
                new Name(operationRequestDataModel.NameOperationType),
                new RequiredStaffBySpecialization(operationRequestDataModel.RequiredStaffBySpecialization),
                new Duration(operationRequestDataModel.Seconds, operationRequestDataModel.Minutes, operationRequestDataModel.Hours, operationRequestDataModel.Days)
            ),
            new DeadlineDate(operationRequestDataModel.DeadlineDate),
            new Priority(operationRequestDataModel.Priority)
        );
    }

    public static OperationRequestDTO FromCreateDTOtoDTO(CreateOperationRequestDTO operationRequestDTO){
        /**
            public MedicalRecordNumber MedicalRecordNumber {get;private set; }
            public LicenceNumber LicenceNumber {get;private set; }
            public OperationType OperationType {get;private set; }
            public DeadlineDate DeadlineDate {get;private set; }
            public Priority Priority {get;private set; }
        */
        
        return new OperationRequestDTO(
           new MedicalRecordNumber( operationRequestDTO.MedicalRecordNumber),
           new LicenceNumber(operationRequestDTO.LicenceNumber),
           new OperationType(
               new Name(operationRequestDTO.Name),
               new RequiredStaffBySpecialization(operationRequestDTO.RequiredStaffBySpecialization),
               operationRequestDTO.EstimatedDuration)
           ,
           new DeadlineDate(operationRequestDTO.DeadlineDate),
           new Priority(operationRequestDTO.Priority)
        );
    }
}