use std::cell::Cell;

use crate::{
    conversion::IntoPyObject, Borrowed, FromPyObject, IntoPy, PyAny, PyObject, Python, ToPyObject,
};

impl<T: Copy + ToPyObject> ToPyObject for Cell<T> {
    fn to_object(&self, py: Python<'_>) -> PyObject {
        self.get().to_object(py)
    }
}

impl<T: Copy + IntoPy<PyObject>> IntoPy<PyObject> for Cell<T> {
    fn into_py(self, py: Python<'_>) -> PyObject {
        self.get().into_py(py)
    }
}

impl<'py, T: Copy + IntoPyObject<'py>> IntoPyObject<'py> for Cell<T> {
    type Target = T::Target;
    type Output = T::Output;
    type Error = T::Error;

    #[inline]
    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.get().into_pyobject(py)
    }
}

impl<'a, 'py, T: Copy + IntoPyObject<'py>> IntoPyObject<'py> for &'a Cell<T> {
    type Target = T::Target;
    type Output = T::Output;
    type Error = T::Error;

    #[inline]
    fn into_pyobject(self, py: Python<'py>) -> Result<Self::Output, Self::Error> {
        self.get().into_pyobject(py)
    }
}

impl<'a, 'py, T: FromPyObject<'a, 'py>> FromPyObject<'a, 'py> for Cell<T> {
    type Error = T::Error;

    fn extract(ob: Borrowed<'a, 'py, PyAny>) -> Result<Self, Self::Error> {
        ob.extract().map(Cell::new)
    }
}
