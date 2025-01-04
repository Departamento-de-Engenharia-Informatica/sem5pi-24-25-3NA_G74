using G74.Domain.Shared;
using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class SpecializationController : ControllerBase
{
    private readonly ISpecializationService _service;

    public SpecializationController(ISpecializationService service)
    {
        _service = service;
    }
    
    // GET: api/Specialization/
    // [Authorize(Roles = "Admin")]
    [HttpGet]
    public async Task<ActionResult<SpecializationDto>> GetSpecialization()
    {
        IEnumerable<SpecializationDto> specializationDto = await _service.GetAll();

        return Ok(specializationDto);
    }
    
    // POST: api/Specialization
    // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
    // [Authorize(Roles = "Admin")]
    [HttpPost]
    public async Task<ActionResult<SpecializationDto>> RegisterSpecialization([FromBody] SpecializationDto specializationDto)
    {
        try
        {
            var retSpecializationDto = await _service.Add(specializationDto);

            return CreatedAtAction(
                nameof(GetSpecializationByCode),
                new { code = retSpecializationDto.Code },
                retSpecializationDto);
        }
        catch (BusinessRuleValidationException ex)
        {
            return BadRequest(new { Message = ex.Message });
        }
    }

    // GET: api/Specialization/code/682468
    // [Authorize(Roles = "Admin")]
    [HttpGet("code/{code}")]
    public async Task<ActionResult<SpecializationDto>> GetSpecializationByCode(long code)
    {
        var specializationDto = await _service.GetByCode(code);

        if (specializationDto == null)
        {
            return NotFound();
        }
        return Ok(specializationDto);
    }
}