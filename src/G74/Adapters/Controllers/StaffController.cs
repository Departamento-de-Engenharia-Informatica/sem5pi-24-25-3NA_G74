using System.ComponentModel.DataAnnotations;
using G74.Domain.Shared;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class StaffController : ControllerBase
{
    private readonly StaffService _service;
    
    public StaffController(StaffService service)
    {
        _service = service;
    }
    
    // GET: api/Staff/
    // [Authorize(Roles = "Admin")]
    [HttpGet]
    public async Task<ActionResult<StaffDto>> GetStaff()
    {
        IEnumerable<StaffDto> staffDto = await _service.GetAll();

        return Ok(staffDto);
    }
    
    // TODO: delete console logs and sql logs in console
    
    // GET: api/Staff//license/682468
    // [Authorize(Roles = "Admin")]
    [HttpGet("license/{licenseNumber}")]
    public async Task<ActionResult<StaffDto>> GetStaffByLicenseNumber(string licenseNumber)
    {
        var staffDto = await _service.GetByLicenseNumber(licenseNumber);

        if (staffDto == null)
        {
            return NotFound();
        }
        return Ok(staffDto);
    }
    
    // POST: api/Staff
    // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
    // [Authorize(Roles = "Admin")]
    [HttpPost]
    public async Task<ActionResult<StaffDto>> RegisterStaff([FromBody] StaffDto staffDto)
    {
        try
        {
            var retStaffDto = await _service.Add(staffDto);
            
            return CreatedAtAction(
                nameof(GetStaffByLicenseNumber),
                new { licenseNumber = retStaffDto.LicenseNumber },
                retStaffDto);
        }
        catch(BusinessRuleValidationException ex)
        {
            return BadRequest(new {Message = ex.Message});
        }
    }
    
    // PUT: api/Staff/license/{licenseNumber}
    // [Authorize(Roles = "Admin")]
    [HttpPut("license/{licenseNumber}")]
    public async Task<ActionResult<StaffDto>> UpdateStaff(string licenseNumber, [FromBody] StaffDto staffDto)
    {
        try
        {
            var staffDtoResult = await _service.Update(licenseNumber, staffDto);
            
            return Ok(staffDtoResult);
        }
        catch (ValidationException ex)
        {
            return BadRequest(new {Message = ex.Message});
        }
        catch(BusinessRuleValidationException ex)
        {
            return NotFound(new {Message = ex.Message});
        }
    }
    
    // [Authorize(Roles = "Admin")]
    [HttpPatch("license/{licenseNumber}/deactivate")]
    public async Task<ActionResult<StaffDto>> DeactivateStaff(string licenseNumber)
    {
        try
        {
            var staffDto = await _service.Deactivate(licenseNumber);
            if (staffDto == null)
            {
                return NotFound($"Staff with license number {licenseNumber} not found.");
            }
            return Ok(staffDto);
        }
        catch (ValidationException ex)
        {
            return BadRequest(ex.Message);
        }
    }
}