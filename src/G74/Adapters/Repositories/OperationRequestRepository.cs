using G74.Domain.Aggregates.OperationType;
using G74.DTO;
using G74.Infrastructure;
using G74.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Adapters.Repositories
{
    public class OperationRequestRepository : BaseRepository<OperationRequestDataModel, Guid>, IOperationRequestRepository
    {
        private readonly BackofficeAppDbContext _context;
        public OperationRequestRepository(BackofficeAppDbContext context) : base(context.OperationRequests)
    {
        _context = context;
    }

        public async Task<OperationRequest> Add(OperationRequestDataModel operation)
        {
             _context.OperationRequests.Add(operation);
            await _context.SaveChangesAsync();
            return  OperationRequestMapper.FromDataModelToDomain(operation);
        }

        public async Task<OperationRequest> Delete(long id)
        {
            var operation = await _context.OperationRequests.FindAsync(id);

            if (operation != null)
            {
                _context.OperationRequests.Remove(operation);
                await _context.SaveChangesAsync();
            }
            else{
                throw new ArgumentException("Id not found.");
            }
            return OperationRequestMapper.FromDataModelToDomain(operation);
        }

        public async Task<OperationRequest> GetOperationRequestByIdAsync(long id)
        {
            var dataOperation = await _context.OperationRequests.FindAsync(id);
    
           
            if (dataOperation == null)
            {
                return null;
            }

            
            var operationRequest = OperationRequestMapper.FromDataModelToDomain(dataOperation);

            return operationRequest;
            
        }

        public async Task<List<OperationRequest>> ReadAll(){
        
            var operationRequestDates = await _context.OperationRequests.ToListAsync();
            var operationRequests = new List<OperationRequest>();

            foreach (var operationRequestDate in operationRequestDates)
            {
                var operationRequest = OperationRequestMapper.FromDataModelToDomain(operationRequestDate);
                operationRequests.Add(operationRequest);
            }

            return operationRequests;
        }

        public async Task<OperationRequest> Update(long id,OperationRequest operation)
        {
            var dataOperation = await _context.OperationRequests.FindAsync(id);
        if (dataOperation == null)
        {
            throw new ArgumentException("No operation found with this ID");
        }

        
            dataOperation.MedicalRecordNumber = long.Parse(operation.MedicalRecordNumber.MedicalNumber);
            dataOperation.LicenceNumber = operation.LicenceNumber.Value;
            dataOperation.OperationTypeId = operation.OperationTypeId;
            dataOperation.DeadlineDate = operation.DeadlineDate.date;
            dataOperation.Priority = operation.Priority.PriorityDescription.ToString();

        
            _context.OperationRequests.Update(dataOperation);
            await _context.SaveChangesAsync();

            return OperationRequestMapper.FromDataModelToDomain(dataOperation);
        }
    
        public async Task<Boolean> GetOperationTypeByIdAsync(int id)
        {
            var list = await _context.OperationTypes.ToListAsync();

            for(var i = 0; i < list.Count; i++)
            {
                Console.WriteLine(list[i].OperationTypeID);
                if(list[i].OperationTypeID == id)
                {
                    
                    return true;
                }
            }

            return false;
            
        }

        

        public async Task ExportOperationRequestDataToProlog()
        {
            //TODO:Terminar Rui Beloto.
        }
    }
}