package com.teenthofabud.laundromat.manager.type.model.validator;

import com.teenthofabud.laundromat.manager.type.model.data.TypeModelDto;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
public class TypeModelDtoValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(TypeModelDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        TypeModelDto dto = (TypeModelDto) target;
        Optional<String> optTypeLovId = dto.getTypeLovId();
        if((/*optTypeLovId != null &&*/ optTypeLovId.isPresent())) {
            boolean isValid = true;
            if(optTypeLovId.get() == null) {
                isValid = false;
            } else {
                try {
                    Long typeLovId = Long.parseLong(optTypeLovId.get());
                    if(typeLovId <= 0L) {
                        isValid = false;
                    }
                } catch (NumberFormatException e) {
                    isValid = false;
                }
            }
            if(!isValid) {
                errors.rejectValue("typeLovId", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            }
            return;
        }
        Optional<String> optName = dto.getName();
        if((/*optName != null &&*/ optName.isPresent()) && StringUtils.isEmpty(optName.get())) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
        Optional<String> optActive = dto.getActive();
        if((/*optActive != null &&*/ optActive.isPresent()) && StringUtils.isEmpty(optActive.get())
            && ((optActive.get().compareToIgnoreCase(Boolean.TRUE.toString()) != 0) || (optActive.get().compareToIgnoreCase(Boolean.FALSE.toString()) != 0))) {
            errors.rejectValue("name", TypeErrorCode.TYPE_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
