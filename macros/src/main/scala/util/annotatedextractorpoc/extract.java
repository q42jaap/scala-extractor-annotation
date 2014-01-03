package util.annotatedextractorpoc;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

@Target(ElementType.PARAMETER)
public @interface extract {
    public String name();
}
