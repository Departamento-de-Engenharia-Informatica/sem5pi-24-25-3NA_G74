using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class OperationTypeController : ControllerBase
{
    private readonly OperationTypeService _operationTypeService;

    public OperationTypeController(OperationTypeService operationTypeService)
    {
        _operationTypeService = operationTypeService;
    }

    //[Authorize(Roles = "Admin,Patient")]
    [HttpPost]
    public async Task<ActionResult<OperationTypeDTO>> RegisterOperationType([FromBody] OperationTypeDTO receivedOperationType)
    {
        if (receivedOperationType == null) return BadRequest("Invalid data, please input operation type data");

        try
        {
            OperationTypeDTO operationTypeDtoReturn = await _operationTypeService.RegisterCreateOperationType(receivedOperationType);

            return CreatedAtAction(nameof(RegisterOperationType), operationTypeDtoReturn);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    //[Authorize(Roles = "Admin")]
    [HttpPatch("update/{operationTypeId}")]
    public async Task<ActionResult<OperationTypeDTO>> UpdateOperationType(
        string operationTypeId,
        [FromBody] OperationTypeDTO operationTypeDtoInfo
    )
    {
        try
        {
            var operationTypeDto = await _operationTypeService.UpdateOperationType(operationTypeId,operationTypeDtoInfo);

            return Ok(new
            {
                message = $"Operation type with operation type id: {operationTypeId} was updated successfully",
                operationTypeDto = operationTypeDto
            });
        }
        catch (Exception e) when (e is InvalidOperationException or ArgumentNullException)
        {
            return NotFound(e.Message);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    //[Authorize(Roles = "Admin,Patient")]
    [HttpDelete("delete/{operationTypeId}")]
    public async Task<IActionResult> DeleteOperationType(string operationTypeId)
    {
        if (string.IsNullOrWhiteSpace(operationTypeId))
        {
            return BadRequest(new { message = "Operation type id cannot be empty or white space" });
        }

        try
        {
            await _operationTypeService.DeleteOperationType(int.Parse(operationTypeId));

            return Ok(new { message = "Operation type deletion with success" });
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    //[Authorize(Roles = "Admin")]
    [HttpGet("type/find")]
    public async Task<IActionResult> ListOperationTypesByFilter([FromQuery] OperationTypeDTO criteria)
    {
        try
        {
            Console.WriteLine($"Criteria: OperationTypeID = {criteria.operationTypeId}, Name = {criteria.name}, Duration = {criteria.duration}");

            var operationTypes = await _operationTypeService.SearchOperationTypeByFilters(criteria);

            if (!operationTypes.Any())
            {
                return NotFound(new { message = "No operation types found matching the search criteria." });
            }

            return Ok(operationTypes);
        }
        catch (Exception ex)
        {
            string errorMessage = "An error occurred searching for operation types: " + ex.Message;
            return BadRequest(errorMessage);
        }
    }

}