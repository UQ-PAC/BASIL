package test_util.tags;

import java.lang.annotation.*;
import org.scalatest.TagAnnotation;

@Inherited
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
public @interface TVSystemTest {}
