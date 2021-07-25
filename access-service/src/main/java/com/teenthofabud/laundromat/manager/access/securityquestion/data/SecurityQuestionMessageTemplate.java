package com.teenthofabud.laundromat.manager.access.securityquestion.data;

import lombok.Getter;

@Getter
public enum SecurityQuestionMessageTemplate {

    MSG_TEMPLATE_SEARCHING_FOR_SECURITY_QUESTION_ENTITY_ID("Searching for SecurityQuestionEntity with id: {}"),
    MSG_TEMPLATE_NO_SECURITY_QUESTION_ENTITY_ID_AVAILABLE("No SecurityQuestionEntity available with id: {}"),
    MSG_TEMPLATE_FOUND_SECURITY_QUESTION_ENTITY_ID("Found SecurityQuestionEntity with id: {}"),
    MSG_TEMPLATE_SECURITY_QUESTION_ID_VALID("security question id: {} is semantically valid"),
    MSG_TEMPLATE_SECURITY_QUESTION_ID_INVALID("security question id: {} is invalid"),
    MSG_TEMPLATE_SECURITY_QUESTION_ID_EMPTY("security question id is empty"),
    MSG_TEMPLATE_SECURITY_QUESTION_EXISTENCE_BY_NAME ("Checking existence of SecurityQuestionEntity with name: {}"),
    MSG_TEMPLATE_SECURITY_QUESTION_EXISTS_BY_NAME ("SecurityQuestionEntity already exists with name: {}"),
    MSG_TEMPLATE_SECURITY_QUESTION_NON_EXISTENCE_BY_NAME ("No SecurityQuestionEntity exists with name: {}");

    private String value;

    private SecurityQuestionMessageTemplate(String value) {
        this.value = value;
    }


}
