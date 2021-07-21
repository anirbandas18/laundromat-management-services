package com.teenthofabud.laundromat.manager.access.usertype.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class UserTypeFormValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(UserTypeForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        UserTypeForm form = (UserTypeForm) target;
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(form.getName()))) {
            log.debug("UserTypeForm.name is empty");
            errors.rejectValue("name", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
