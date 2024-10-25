

using Microsoft.AspNetCore.Mvc;

[Route("api/[controller]")]
[ApiController]
public class OperationRequestController : ControllerBase
{
    private readonly IAppServiceOperationRequest _appServiceOperationRequest;

    public OperationRequestController(IAppServiceOperationRequest appServiceOperationRequest)
    {
        _appServiceOperationRequest = appServiceOperationRequest;
    }

    [HttpPost]
    public async Task<ActionResult<OperationRequestDTO>> RegisterOperationRequest([FromBody] CreateOperationRequestDTO receivedOperationRequest)
    {
        
        if (receivedOperationRequest == null)
        { 
            Console.WriteLine("Cheguei");
            return BadRequest("Invalid data, please input patient data");
        }
        try 
        {
            OperationRequestDTO operationReturnDto = await _appServiceOperationRequest.RegisterOperationRequest(receivedOperationRequest);

            return CreatedAtAction(nameof(RegisterOperationRequest), operationReturnDto);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
        
    }

}