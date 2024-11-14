    

using Microsoft.AspNetCore.Authorization;
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

    //[Authorize(Roles = "Doctor")]
    [HttpPost]
    public async Task<ActionResult<OperationRequestDTO>> RegisterOperationRequest([FromBody] CreateOperationRequestDTO receivedOperationRequest)
    {
        
        if (receivedOperationRequest == null)
        { 
            return BadRequest("Invalid data, please input patient data");
        }
        try 
        {
            Console.WriteLine("Received Operation Request: " + receivedOperationRequest);
            OperationRequestDTO operationReturnDto = await _appServiceOperationRequest.RegisterOperationRequest(receivedOperationRequest);

            return CreatedAtAction(nameof(RegisterOperationRequest), operationReturnDto);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
        
    }
    
    //[Authorize(Roles = "Doctor")]
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
            
            var operationDTO = await _appServiceOperationRequest.UpdateOperationRequest(operationRequestId,OperationRequestMapper.FromCreateDTOtoDTO(updatedOperationDto));
            
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
    
    //[Authorize(Roles = "Doctor")]
    [HttpDelete("{id}")]
    public async Task<ActionResult<OperationRequestDTO>> DeleteOperationRequest(
        string id
    )
    {
        if (!Guid.TryParse(id, out Guid operationRequestId))
        {
            return BadRequest("Invalid ID format.");
        }
        try{
            var operationDTO = await _appServiceOperationRequest.DeleteOperationRequest(operationRequestId);
            return NoContent();
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
    
    //[Authorize(Roles = "Doctor")]
    [HttpGet]
    public async Task<ActionResult<OperationRequestDTO>> GetAllOperationRequest()
    {
        try{
            var list = await _appServiceOperationRequest.Read();
            return Ok(list);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
    }

}