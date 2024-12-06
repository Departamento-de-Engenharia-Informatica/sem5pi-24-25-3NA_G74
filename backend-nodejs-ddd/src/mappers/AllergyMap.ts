import {Mapper} from "../core/infra/Mapper";
import {Allergy} from "../domain/Allergy";
import {IAllergyDTO} from "../dto/IAllergyDTO";

export class AllergyMap extends Mapper<Allergy> {


    public static toDTO(allergy: Allergy): IAllergyDTO {
        return {
            id: allergy.id.toString(),
            designation: allergy.designation
        } as IAllergyDTO;
    }

    
}