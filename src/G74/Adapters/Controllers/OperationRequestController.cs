

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

    [HttpPut("{id}")]
    public async Task<ActionResult<OperationRequestDTO>> UpdateOperationRequest(
            string id,
            [FromBody] CreateOperationRequestDTO updatedOperationDto
        )
    {
        
        
        if (updatedOperationDto == null)
        {
            return BadRequest("Invalid operation data.");
        }
        if (!Guid.TryParse(id, out Guid operationRequestId))
        {
            return BadRequest("Invalid ID format.");
        }

        try
        {
            
            var patientDTO = await _appServiceOperationRequest.UpdateOperationRequest(operationRequestId,OperationRequestMapper.FromCreateDTOtoDTO(updatedOperationDto));
            
            return Ok(updatedOperationDto);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }


}