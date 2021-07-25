package com.teenthofabud.laundromat.manager.access.securityquestion.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class SecurityQuestionDtoValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(SecurityQuestionDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        SecurityQuestionDto dto = (SecurityQuestionDto) target;
        Optional<String> optName = dto.getName();
        if(optName.isPresent() && StringUtils.isEmpty(StringUtils.trimWhitespace(optName.get()))) {
            errors.rejectValue("name", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            log.debug("SecurityQuestionDto.name is invalid");
            return;
        }
        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                log.debug("SecurityQuestionDto.active is invalid");
                return;
            }
        }
    }
}
