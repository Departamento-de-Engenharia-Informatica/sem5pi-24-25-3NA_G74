

using G74.Domain.Aggregates.Patient;
using G74.Domain.Shared;

namespace G74.Domain;

public class OpertaionRequest : Entity<OperationRequestId>, IAggregateRoot
{
    public PatientId patientID { get; private set;}
    public DoctorId doctorId { get; private set;}
    public OperationTypeId operationTypeId { get; private set;}
    public DeadlineDate deadlineDate { get; private set;}
    public Priority priority { get; private set;}
    

}