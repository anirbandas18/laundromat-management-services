package com.teenthofabud.laundromat.manager.tax.lov.data;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABError;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TaxLOVException extends TOABBaseException {

    @ToString.Include
    private transient TOABError error;

    public TaxLOVException(String message) {
        super(message);
    }

    public TaxLOVException(String message, Object[] parameters) {
        super(message, parameters);
    }

    public TaxLOVException(TOABError error, String message, Object[] parameters) {
        super(error, message, parameters);
        this.error = error;
    }

    public TaxLOVException(TOABError error, Object[] parameters) {
        super(error, parameters);
        this.error = error;
    }

    @Override
    public String getSubDomain() {
        return "LOV";
    }

}
