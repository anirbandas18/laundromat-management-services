package com.teenthofabud.laundromat.manager.type.lov.validator;

import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVDto;
import com.teenthofabud.laundromat.manager.error.TypeErrorCode;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
public class TypeLOVDtoValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeLOVDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TypeLOVDto dto = (TypeLOVDto) target;
        Optional<String> optName = dto.getName();
        if((optName != null && optName.isPresent()) && StringUtils.isEmpty(optName.get())) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
        Optional<String> optActive = dto.getActive();
        if((optActive != null && optActive.isPresent()) && StringUtils.isEmpty(optActive.get())
            && ((optActive.get().compareToIgnoreCase(Boolean.TRUE.toString()) != 0) || (optActive.get().compareToIgnoreCase(Boolean.FALSE.toString()) != 0))) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
