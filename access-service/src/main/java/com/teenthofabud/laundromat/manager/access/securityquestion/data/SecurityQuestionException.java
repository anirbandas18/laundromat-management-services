package com.teenthofabud.laundromat.manager.access.securityquestion.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class SecurityQuestionException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public SecurityQuestionException(String message) {
        super(message);
    }

    public SecurityQuestionException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public SecurityQuestionException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public SecurityQuestionException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "Security Question";
    }

}
