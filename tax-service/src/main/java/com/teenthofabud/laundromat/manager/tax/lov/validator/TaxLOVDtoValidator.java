package com.teenthofabud.laundromat.manager.tax.lov.validator;

import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.data.TaxLOVDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class TaxLOVDtoValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TaxLOVDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TaxLOVDto dto = (TaxLOVDto) target;
        Optional<String> optName = dto.getName();
        if(optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
            log.debug("TaxLOVDto.name is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", TaxErrorCode.TAX_ATTRIBUTE_INVALID.name());
                log.debug("TaxLOVDto.active is invalid");
                return;
            }
        }
    }
}
