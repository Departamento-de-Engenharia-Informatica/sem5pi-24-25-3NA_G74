import {AllergyDTO} from '../dto/allergy.dto';
import {Allergy} from '../domain/models/allergy.model';


export class AllergyMapper {

    public static toDomain(allergyDTO: AllergyDTO): Allergy {

        const designation = allergyDTO.designation? allergyDTO.designation : "UNAVAILABLE_DESIGNATION";

        const description = allergyDTO.description? allergyDTO.description : "UNAVAILABLE_DESCRIPTION";

        return {
            code: allergyDTO.code,
            designation: designation,
            description: description
        };

    }

    public static toDto(allergy: Allergy): AllergyDTO {
        return {
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description
        };
    }
}
