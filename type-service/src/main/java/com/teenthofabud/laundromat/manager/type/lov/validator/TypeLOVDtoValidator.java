package com.teenthofabud.laundromat.manager.type.lov.validator;

import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVDto;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class TypeLOVDtoValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeLOVDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TypeLOVDto dto = (TypeLOVDto) target;
        Optional<String> optName = dto.getName();
        if(optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            log.debug("TypeLOVDto.name is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
                log.debug("TypeLOVDto.active is invalid");
                return;
            }
        }
    }
}
