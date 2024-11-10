namespace G74.Domain.IRepositories;

public interface ISurgeryRoomRepository
{
    Task<bool> SurgeryRoomExists(string id);
}