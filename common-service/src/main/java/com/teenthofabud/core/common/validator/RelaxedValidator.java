package com.teenthofabud.core.common.validator;

import org.springframework.validation.Errors;

@FunctionalInterface
public interface RelaxedValidator<T> {

    public Boolean validateLoosely(T t, Errors errors);

}
