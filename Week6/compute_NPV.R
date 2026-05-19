compute_NPV = function (value, time, discount) 
{
  # use value to compute NPV
  result = value/(1 + discount)^time
  return(result)
}